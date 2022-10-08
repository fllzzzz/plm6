<template>
  <el-tag v-if="detail.applicant" type="success" effect="dark">
    {{ `申请人：${detail.applicant.name} | ${detail.applicant.deptName}` }}
  </el-tag>
  <el-tag effect="plain">{{ `退库申请时间：${parseTime(detail.createTime)}` }}</el-tag>
  <el-tag effect="plain">{{ detail.boolRealReturn ? '真实' : '虚拟' }}退库</el-tag>
</template>

<script setup>
import { defineProps } from 'vue'
import { parseTime } from '@/utils/date'

// eslint-disable-next-line no-unused-vars
const props = defineProps({
  detail: {
    type: Object,
    default: () => {
      return {}
    }
  }
})
</script>

