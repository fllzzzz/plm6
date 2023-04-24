<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">APP任务上报重量配置</span>
        <common-tip-button
          v-permission="permission.foldingPriceMethodEdit"
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
      <el-form-item label="上报页面" prop="boolShowWeightInManufacture">
        <el-radio-group v-model="form.boolShowWeightInManufacture">
          <el-radio :label="true">显示</el-radio>
          <el-radio :label="false">不显示</el-radio>
        </el-radio-group>
      </el-form-item>
      <el-form-item label="上报记录页面" prop="boolShowWeightInRecord">
        <el-radio-group v-model="form.boolShowWeightInRecord">
          <el-radio :label="true">显示</el-radio>
          <el-radio :label="false">不显示</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getTaskReport, setTaskReport } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  boolShowWeightInManufacture: undefined,
  boolShowWeightInRecord: undefined
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
    const { boolShowWeightInManufacture = false, boolShowWeightInRecord = false } = await getTaskReport()
    form.value = { boolShowWeightInManufacture, boolShowWeightInRecord }
    dataSource.value = { boolShowWeightInManufacture, boolShowWeightInRecord }
  } catch (error) {
    console.log('获取APP任务上报重量配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setTaskReport(form.value)
    ElNotification({
      title: 'APP任务上报重量配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置APP任务上报重量配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
