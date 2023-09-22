<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">价格录入办理方式</span>
        <common-tip-button
          v-permission="permission.contractPriceConfigEdit"
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
      <el-form-item label="办理方式" prop="type">
        <el-radio-group v-model="form.priceEditMode">
          <el-radio :label="1">价格录入即审核</el-radio>
          <el-radio :label="2">价格录入即保存</el-radio>
        </el-radio-group>
      </el-form-item>
       <el-form-item label-width="0">
        <span class="form-item-tip">
          价格录入即审核：价格录入保存后，即在变更记录生成该条价格录入的审批单，审批完成则价格修改成功；
        </span>
        <span class="form-item-tip">价格录入即保存：增加中转保存功能，录入价格的构件不会直接生成审批单，
而是在“保存记录”按钮中，对已保存的构件价格批量办理审核；</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getPriceConfig, setPriceConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  priceEditMode: undefined
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
    const data = await getPriceConfig()
    const priceEditMode = data?.priceEditMode
    form.value = { priceEditMode }
    dataSource.value = { priceEditMode }
  } catch (error) {
    console.log('获取价格办理方式', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setPriceConfig(form.value)
    ElNotification({
      title: '配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置价格办理方式', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
