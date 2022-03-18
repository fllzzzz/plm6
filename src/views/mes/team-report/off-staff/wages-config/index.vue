<template>
  <wages-adjust :organizationType="teamAttributeEnum.OFF_STAFF.V"></wages-adjust>
</template>

<script setup>
import { provide } from 'vue'
import wagesAdjust from '../../wages-adjust'
import { teamAttributeEnum } from '@enum-ms/mes'
import { offStaffWagesAdjustPM as permission } from '@/page-permission/mes'

provide('permission', permission)
</script>
