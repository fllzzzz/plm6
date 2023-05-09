<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">零件排产车间、产线、生产组配置</span>
        <common-tip-button
          v-permission="permission.machinePartSchedulingEdit"
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
      <el-form-item label="显示层级" prop="type">
        <el-radio-group v-model="form.type">
          <el-radio :label="typeEnum.NO.V">无</el-radio>
          <el-radio :label="typeEnum.WORKSHOP.V">车间级</el-radio>
          <el-radio :label="typeEnum.PRODUCTION_LINE.V">产线级</el-radio>
          <el-radio :label="typeEnum.GROUPS.V">生产组级</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getInfo, setInfo } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'
import { machinePartSchedulingTypeEnum as typeEnum } from '@enum-ms/mes'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  type: undefined
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
    const type = await getInfo()
    form.value = { type }
    dataSource.value = { type }
  } catch (error) {
    console.log('获取零件排产配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setInfo(form.value)
    ElNotification({
      title: '零件排产配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置零件排产配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
