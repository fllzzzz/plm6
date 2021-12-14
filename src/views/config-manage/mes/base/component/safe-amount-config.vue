<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <el-tooltip class="item" effect="light" :content="`安全余额 =（合同金额 - 累计发货额）* 安全余额比例`" placement="top">
          <div style="display: inline-block; cursor: pointer">
            <span class="card-title">安全余额配置</span>
            <i class="el-icon-info" style="color: #909399;margin-left:5px;"/>
          </div>
        </el-tooltip>
        <common-tip-button
          v-permission="permission.safeAmountFactorEdit"
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
    <el-form ref="formRef" v-loading="dataLoading" :model="form" :rules="rules" :disabled="formDisabled" label-width="100px">
      <el-form-item label="加工订单" prop="safetyFactor">
        <el-input-number
          v-if="checkPermission(permission.safeAmountFactorEdit)"
          v-model.number="form.safetyFactor"
          :min="0"
          :max="100"
          :step="1"
          :precision="2"
          placeholder="比例"
          :controls="false"
          style="width: 80px"
          class="input-underline"
        />
        <span v-else>{{ form.safetyFactor }}</span>
        %
      </el-form-item>
      <el-form-item label="项目承包">
        <el-tag type="info">项目承包不设安全余额</el-tag>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getSafetyFactor as getConfig, setSafetyFactor as setConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'
import checkPermission from '@/utils/system/check-permission'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

const rules = {
  safetyFactor: [{ required: true, message: '请选择过磅短信接收人', trigger: 'blur' }]
}
const formRef = ref()
// 数据源
const dataSource = ref({
  safetyFactor: undefined
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
  dataLoading.value = true
  try {
    const { safetyFactor } = await getConfig()
    form.value = { safetyFactor }
    dataSource.value = { safetyFactor }
  } catch (error) {
    console.log('获取安全余额配置失败', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  try {
    const passed = await formRef.value.validate()
    if (!passed) return
    submitLoading.value = true
    await setConfig(form.value)
    ElNotification({
      title: '安全余额配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置安全余额配置失败', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
