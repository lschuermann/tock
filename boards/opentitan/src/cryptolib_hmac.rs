use kernel::platform::chip::Chip;
use kernel::utilities::cells::{MapCell, OptionalCell, TakeCell};
use kernel::utilities::leasable_buffer::{LeasableBuffer, LeasableMutableBuffer, LeasableBufferDynamic};
use kernel::ErrorCode;
use kernel::deferred_call::{DeferredCall, DeferredCallClient};
use core::cell::Cell;

pub const SHA_256_OUTPUT_LEN_BYTES: usize = 32;

#[derive(Copy, Clone, Debug)]
enum DeferredCallOp {
    AddData,
    Run,
}

pub struct CryptolibHmac<'client, 'chip, C: Chip> {
    cryptosvc: contsvc::ContSvc<'chip, C>,
    // data_client: OptionalCell<&'client dyn kernel::hil::digest::ClientData<SHA_256_OUTPUT_LEN_BYTES>>,
    // hash_client: OptionalCell<&'client dyn kernel::hil::digest::ClientHash<SHA_256_OUTPUT_LEN_BYTES>>,
    client: OptionalCell<&'client dyn kernel::hil::digest::Client<SHA_256_OUTPUT_LEN_BYTES>>,
    cryptolib_hmac_context: MapCell<crate::cryptolib_mac::hmac_context_t>,
    input_data: OptionalCell<LeasableBufferDynamic<'static, u8>>,
    digest_buffer: TakeCell<'static, [u8; 32]>,
    deferred_call: DeferredCall,
    deferred_call_op: Cell<DeferredCallOp>,
}

impl<'client, 'chip, C: Chip> CryptolibHmac<'client, 'chip, C> {
    pub fn new(cryptosvc: contsvc::ContSvc<'chip, C>) -> Self {
        CryptolibHmac {
            cryptosvc,
            // data_client: OptionalCell::empty(),
	    // hash_client: OptionalCell::empty(),
	    client: OptionalCell::empty(),
            cryptolib_hmac_context: MapCell::empty(),
            input_data: OptionalCell::empty(),
	    digest_buffer: TakeCell::empty(),
            deferred_call: DeferredCall::new(),
	    deferred_call_op: Cell::new(DeferredCallOp::AddData),
        }
    }

    fn with_hmac_context<R>(
        &self,
        fun: impl FnOnce(*mut crate::cryptolib_mac::hmac_context_t) -> R,
    ) -> Result<R, ErrorCode> {
        // Return early if not initialized:
        if self.cryptolib_hmac_context.is_none() {
	    return Err(ErrorCode::RESERVE);
        }

        self.cryptolib_hmac_context
            .map(|hmac_context_rust| {
                self.cryptosvc
                    .allocate_stacked_t::<crate::cryptolib_mac::hmac_context_t, _, _>(
                        |hmac_context_ptr| {
                            {
                                let hmac_context: &mut crate::cryptolib_mac::hmac_context_t =
                                    unsafe { &mut *hmac_context_ptr };
                                hmac_context.data.copy_from_slice(&hmac_context_rust.data);
                            }

                            let res = fun(hmac_context_ptr);

                            {
                                let hmac_context: &crate::cryptolib_mac::hmac_context_t =
                                    unsafe { &*hmac_context_ptr };
                                hmac_context_rust.data.copy_from_slice(&hmac_context.data);
                            }

                            Ok(res)
                        },
                    )
            })
            .ok_or(ErrorCode::FAIL)??
    }

    pub fn hmac_init(&self, key: &[u8]) -> Result<(), ErrorCode> {
	self.cryptosvc
            .allocate_stacked_t::<crate::cryptolib_mac::hmac_context_t, _, _>(
                |hmac_context_ptr| {
		    self.cryptosvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_blinded_key_t, _, _>(|blinded_key_ptr| {
			// Set the blinded key config prior to calling the `keyblob_num_words` helper:
			let config_ptr = {
			    let blinded_key: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key_ptr };

			    // Set the blinded key config first:
			    blinded_key.config = crate::cryptolib_mac::crypto_key_config {
				version: crate::cryptolib_mac::crypto_lib_version_kCryptoLibVersion1,
				key_mode: crate::cryptolib_mac::key_mode_kKeyModeHmacSha256,
				key_length: 32, // HMAC-SHA256
				hw_backed: crate::cryptolib_mac::hardened_bool_kHardenedBoolFalse,
				diversification_hw_backed: crate::cryptolib_mac::crypto_const_uint8_buf_t {
				    data: core::ptr::null(),
				    len: 0,
				},
				exportable: crate::cryptolib_mac::hardened_bool_kHardenedBoolFalse,
				security_level: crate::cryptolib_mac::crypto_key_security_level_kSecurityLevelLow,
			    };

			    &mut blinded_key.config as *mut crate::cryptolib_mac::crypto_key_config
			};

			let (keyblob_words, _) = self.cryptosvc.invoke_service(
			    // keyblob_num_words (wrapped)
			    self.cryptosvc.resolve_function_pointer(1).unwrap(),
			    blinded_key_ptr as usize, 0, 0, 0, 0, 0, 0, 0, false
			).unwrap();

			self.cryptosvc.allocate_stacked_array::<17, u32, _, _>(|test_mask_ptr| {
			    {
				(unsafe { &mut *test_mask_ptr }).copy_from_slice(&[
				    0x8cb847c3, 0xc6d34f36, 0x72edbf7b, 0x9bc0317f, 0x8f003c7f, 0x1d7ba049,
				    0xfd463b63, 0xbb720c44, 0x784c215e, 0xeb101d65, 0x35beb911, 0xab481345,
				    0xa7ebc3e3, 0x04b2a1b9, 0x764a9630, 0x78b8f9c5, 0x3f2a1d8e,
				]);
			    }

			    self.cryptosvc.allocate_stacked_array::<8, u32, _, _>(|test_key_ptr| {
				{
				    let mut key_iter = key.iter();
				    for key_word in (unsafe { &mut *test_key_ptr }).iter_mut() {
					let word = u32::from_le_bytes([
					    *key_iter.next().unwrap_or(&0),
					    *key_iter.next().unwrap_or(&0),
					    *key_iter.next().unwrap_or(&0),
					    *key_iter.next().unwrap_or(&0),
					]);

					*key_word = word;
				    }
				}

				self.cryptosvc.allocate_stacked_slice::<u32, _, _>(keyblob_words, |keyblob_slice_ptr| {
				    let keyblob_res =  self.cryptosvc.invoke_service(
					// keyblob_from_key_and_mask (wrapped)
					self.cryptosvc.resolve_function_pointer(5).unwrap(),
					test_key_ptr as *mut u32 as usize,
					test_mask_ptr as *mut u32 as usize,
					config_ptr as usize,
					keyblob_slice_ptr as *mut u32 as usize,
					0, 0, 0, 0, false
				    ).unwrap();

				    {
					let blinded_key: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key_ptr };
					blinded_key.keyblob = keyblob_slice_ptr as *mut u32;
					blinded_key.keyblob_length = keyblob_words * core::mem::size_of::<u32>();
					blinded_key.checksum = 0;
				    }

				    let (checksum, _) =  self.cryptosvc.invoke_service(
					// integrity_blinded_checksum
					self.cryptosvc.resolve_function_pointer(14).unwrap(),
					blinded_key_ptr as usize,
					0, 0, 0, 0, 0, 0, 0, false
				    ).unwrap();

				    {
					let blinded_key: &mut crate::cryptolib_mac::crypto_blinded_key_t  = unsafe { &mut *blinded_key_ptr };
					blinded_key.checksum = checksum as u32;
				    }

				    self.cryptosvc
					.invoke_service(
					    // otcrypto_hmac_init
					    self.cryptosvc
						.resolve_function_pointer(10)
						.unwrap(),
					    hmac_context_ptr as usize,
					    blinded_key_ptr as usize,
					    0,
					    0,
					    0,
					    0,
					    0,
					    0,
					    false,
					)
					.unwrap()
				}).unwrap()
			    }).unwrap()
			}).unwrap()
		    }).unwrap();


                    let hmac_context: &crate::cryptolib_mac::hmac_context_t =
                        unsafe { &*hmac_context_ptr };
                    self.cryptolib_hmac_context.put(hmac_context.clone());

                    Ok::<(), ErrorCode>(())
                },
            )
            .map_err(|_| ErrorCode::NOMEM)??;

	Ok(())
    }

    pub fn hmac_reset(&self) {
	self.cryptolib_hmac_context.take();
    }

    pub fn hmac_update(&self, data: &[u8]) -> Result<(), ErrorCode> {
        self.with_hmac_context(|hmac_context_ptr| {
            self.cryptosvc.allocate_stacked_slice::<u8, _, _>(data.len(), |msg_ptr| {
                (unsafe { &mut *msg_ptr }).copy_from_slice(data);

                self.cryptosvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_uint8_buf_t, _, _>(|msg_buf_ptr| {
                    {
                        let msg_buf: &mut crate::cryptolib_mac::crypto_uint8_buf_t = unsafe { &mut *msg_buf_ptr };
                        msg_buf.data = msg_ptr as *mut u8;
                        msg_buf.len = data.len();
                    }

                    self.cryptosvc.invoke_service(
                        // otcrypto_hmac_final
                        self.cryptosvc.resolve_function_pointer(11).unwrap(),
                        hmac_context_ptr as usize,
                        msg_buf_ptr as usize,
                        0, 0, 0, 0, 0, 0, false
                    ).unwrap();

                    // panic!("Keyblob res: {:x?} {:x?}, Checksum: {:x?}, hmac res {:x?}, {:p} {:p} [{:p} {:p} {:p}], key mode {:?}", keyblob_res, {&*keyblob_slice_ptr}, checksum, res, blinded_key_ptr, (unsafe { &* blinded_key_ptr}).keyblob, keyblob_ptr_0, keyblob_ptr_1, keyblob_slice_ptr, &(unsafe { &* blinded_key_ptr}).config.key_mode);

                    
                }).unwrap()
            }).unwrap()
        }).unwrap();

	Ok(())
    }

    pub fn hmac_finalize(&self, output: &mut [u8]) {
	self.with_hmac_context(|hmac_context_ptr| {
	    self.cryptosvc.allocate_stacked_array::<{256 / 32}, u32, _, _>(|act_tag_ptr| {
		self.cryptosvc.allocate_stacked_t::<crate::cryptolib_mac::crypto_uint8_buf_t, _, _>(|tag_buf_ptr| {
		    {
			let tag_buf: &mut crate::cryptolib_mac::crypto_uint8_buf_t = unsafe { &mut *tag_buf_ptr };
			tag_buf.data = act_tag_ptr as *mut u32 as *mut u8;
			tag_buf.len = 256 / 8;
		    }

		    self.cryptosvc.invoke_service(
			// otcrypto_hmac_final
			self.cryptosvc.resolve_function_pointer(12).unwrap(),
			hmac_context_ptr as usize,
			tag_buf_ptr as usize,
			0, 0, 0, 0, 0, 0, false
                    ).unwrap();

		    (unsafe { &*act_tag_ptr })
			.iter()
			.flat_map(|word| word.to_le_bytes())
			.zip(output.iter_mut())
			.for_each(|(tag_byte, out_byte)| *out_byte = tag_byte);
		}).unwrap()
	    }).unwrap();
	}).unwrap();

	self.hmac_reset();
    }
}

impl<'client, 'chip, C: Chip> kernel::hil::digest::DigestData<'client, 32> for CryptolibHmac<'client, 'chip, C> {
    fn set_data_client(&'client self, client: &'client dyn kernel::hil::digest::ClientData<32>) {
	// self.data_client.set(client);
	unimplemented!()
    }
	
    fn add_data(
        &self,
        data: LeasableBuffer<'static, u8>,
    ) -> Result<(), (ErrorCode, LeasableBuffer<'static, u8>)> {
        match self.hmac_update(data.active_slice()) {
            Ok(_) => {
                // Schedule deferred call and save data
		self.input_data.replace(LeasableBufferDynamic::Immutable(data));
		self.deferred_call_op.set(DeferredCallOp::AddData);
		self.deferred_call.set();

		Ok(())
            }
            Err(e) => Err((e, data)),
        }
    }

    fn add_mut_data(
	&self,
	data: LeasableMutableBuffer<'static, u8>,
    ) -> Result<(), (ErrorCode, LeasableMutableBuffer<'static, u8>)> {
        match self.hmac_update(data.active_slice()) {
            Ok(_) => {
                // Schedule deferred call and save data
		self.input_data.replace(LeasableBufferDynamic::Mutable(data));
		self.deferred_call_op.set(DeferredCallOp::AddData);
		self.deferred_call.set();

		Ok(())
            }
            Err(e) => Err((e, data)),
        }
    }

    fn clear_data(&self) {
	self.hmac_reset();
    }
}

impl<'client, 'chip, C: Chip> kernel::hil::digest::DigestHash<'client, 32> for CryptolibHmac<'client, 'chip, C> {
    fn run(
	&self,
	digest: &'static mut [u8; 32],
    ) -> Result<(), (ErrorCode, &'static mut [u8; 32])> {
	self.hmac_finalize(digest);

	// Schedule deferred call and save data
	self.digest_buffer.replace(digest);
	self.deferred_call_op.set(DeferredCallOp::Run);
	self.deferred_call.set();

	Ok(())
    }

    fn set_hash_client(&'client self, client: &'client dyn kernel::hil::digest::ClientHash<32>) {
        // self.hash_client.set(client);
	unimplemented!()
    }
}

impl<'client, 'chip, C: Chip> kernel::hil::digest::DigestVerify<'client, 32> for CryptolibHmac<'client, 'chip, C> {
    fn verify(
	&self,
	compare: &'static mut [u8; 32],
    ) -> Result<(), (ErrorCode, &'static mut [u8; 32])> {
	unimplemented!()
    }

    fn set_verify_client(&'client self, _client: &'client dyn kernel::hil::digest::ClientVerify<32>) {
        unimplemented!()
    }
}

// impl<'client, 'chip, C: Chip> kernel::hil::digest::DigestDataHash<'client, 32> for CryptolibHmac<'client, 'chip, C> {
//     // fn set_client(&'client self, client: &'client dyn kernel::hil::digest::Client<32>) {
//     // 	self.client.set(client);
//     // }
// }

impl<'client, 'chip, C: Chip> kernel::hil::digest::Digest<'client, 32> for CryptolibHmac<'client, 'chip, C> {
    fn set_client(&'client self, client: &'client dyn kernel::hil::digest::Client<32>) {
	self.client.set(client);
    }
}

impl<'client, 'chip, C: Chip> kernel::hil::digest::HmacSha256 for CryptolibHmac<'client, 'chip, C> {
    fn set_mode_hmacsha256(&self, key: &[u8]) -> Result<(), ErrorCode> {
	self.hmac_init(key).unwrap();
	Ok(())
    }
}

impl<'client, 'chip, C: Chip> kernel::hil::digest::HmacSha384 for CryptolibHmac<'client, 'chip, C> {
    fn set_mode_hmacsha384(&self, _key: &[u8]) -> Result<(), ErrorCode> {
	unimplemented!()
    }
}

impl<'client, 'chip, C: Chip> kernel::hil::digest::HmacSha512 for CryptolibHmac<'client, 'chip, C> {
    fn set_mode_hmacsha512(&self, _key: &[u8]) -> Result<(), ErrorCode> {
	unimplemented!()
    }
}

impl<'client, 'chip, C: Chip> DeferredCallClient for CryptolibHmac<'client, 'chip, C> {
    fn handle_deferred_call(&self) {
	match self.deferred_call_op.get() {
	    DeferredCallOp::AddData => {
		let data_buf = self.input_data.take().unwrap();
		match data_buf {
		    LeasableBufferDynamic::Immutable(buf) => {
			self.client.map(move |c| c.add_data_done(Ok(()), buf));
		    },
		    LeasableBufferDynamic::Mutable(buf) => {
			self.client.map(move |c| c.add_mut_data_done(Ok(()), buf));
		    },
		}
	    },
	    DeferredCallOp::Run => {
		let digest_buf = self.digest_buffer.take().unwrap();
		self.client.map(move |c| c.hash_done(Ok(()), digest_buf));
	    }
	}
    }

    fn register(&'static self) {
        self.deferred_call.register(self);
    }
}
