<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">围护-折边件计价方式配置</span>
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
      <el-form-item label="选择计价方式" prop="priceType">
        <el-radio-group v-model="form.priceType">
          <el-radio :label="true">单价*计价单位</el-radio>
          <el-radio :label="false">单价*计价单位*折弯次数</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getFoldingPriceMethod as getConfig, setFoldingPriceMethod as setConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  priceType: undefined
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
    const { priceType = false } = await getConfig()
    form.value = { priceType }
    dataSource.value = { priceType }
  } catch (error) {
    console.log('获取折边件计价方式填写配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setConfig(form.value)
    ElNotification({
      title: '折边件计价方式配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置折边件计价方式配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
