// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2024.

use kernel::platform::chip::Chip;
use kernel::process::Process;
use kernel::process::ShortId;
use kernel::storage_permissions::StoragePermissions;

/// Assign storage permissions that grant applications access to their own
/// state.
pub struct IndividualStoragePermissions<C: Chip> {
    _chip: core::marker::PhantomData<C>,
}

impl<C: Chip> IndividualStoragePermissions<C> {
    pub fn new() -> Self {
        Self {
            _chip: core::marker::PhantomData,
        }
    }
}

impl<C: Chip> kernel::process::ProcessStandardStoragePermissionsPolicy<C>
    for IndividualStoragePermissions<C>
{
    fn get_permissions(&self, process: &kernel::process::ProcessStandard<C>) -> StoragePermissions {
        // If we have a fixed ShortId then this process can have storage
        // permissions. Otherwise we get null permissions.
        match process.short_app_id() {
            ShortId::Fixed(id) => StoragePermissions::new_self_only(id),
            ShortId::LocallyUnique => StoragePermissions::new_null(),
        }
    }
}
