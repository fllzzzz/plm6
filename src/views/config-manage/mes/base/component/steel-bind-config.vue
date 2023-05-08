<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">钢板绑定配置</span>
        <common-tip-button
          v-permission="permission.steelBindConfigEdit"
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
      <el-form-item label="是否绑定钢板" prop="steelPlateType">
        <el-radio-group v-model="form.steelPlateType">
          <el-radio :label="true">是</el-radio>
          <el-radio :label="false">否</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getSteelType, setSteelType } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  steelPlateType: undefined
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
    const { steelPlateType = false } = await getSteelType()
    form.value = { steelPlateType }
    dataSource.value = { steelPlateType }
  } catch (error) {
    console.log('获取绑定钢板配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setSteelType(form.value)
    ElNotification({
      title: '绑定钢板配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置绑定钢板配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
