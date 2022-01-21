<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">移动端打印配置</span>
        <common-tip-button
          v-permission="permission.appPrintConfigEdit"
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
      <el-form-item label="围护" prop="enclosureType">
        <el-radio-group v-model="form.enclosureType">
          <el-radio :label="1">基础型</el-radio>
          <el-radio :label="2">极简型</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getPrintConfig as getConfig, setPrintConfig as setConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  enclosureType: undefined
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
    const { enclosureType } = await getConfig()
    form.value = { enclosureType }
    dataSource.value = { enclosureType }
  } catch (error) {
    console.log('获取移动端打印配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setConfig(form.value)
    ElNotification({
      title: '移动端打印配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置移动端打印配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
