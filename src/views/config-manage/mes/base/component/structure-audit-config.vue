<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">构件、部件特征定义配置</span>
        <common-tip-button
          v-permission="permission.machinePartStructureEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          type="primary"
          style="float: right"
          @click="submit"
        >
          保存
        </common-tip-button>
      </div>
    </template>
    <el-form v-loading="dataLoading" :model="form" :disabled="formDisabled" label-width="100px">
      <el-form-item label="是否审批" prop="type">
        <el-radio-group v-model="form.auditType">
          <el-radio :label="true">是</el-radio>
          <el-radio :label="false">否</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getAuditConfig, setAuditConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  auditType: undefined
})
// 表单
const form = ref(dataSource.value)
// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

const submitDisabled = computed(() => isObjectValueEqual(form.value, dataSource.value))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

onMounted(() => {
  fetchData()
})

async function fetchData() {
  if (!checkPermission(permission.machinePartStructureGet)) {
    return
  }
  dataLoading.value = true
  try {
    const data = await getAuditConfig()
    const auditType = data?.auditType
    form.value = { auditType }
    dataSource.value = { auditType }
  } catch (error) {
    console.log('获取构件部件特征定义审批配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setAuditConfig(form.value)
    ElNotification({
      title: '配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置构件部件特征定义审批配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
