<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">物流信息填写配置</span>
        <common-tip-button
          v-permission="permission.driverFillConfigEdit"
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
      <el-form-item label="选择配置" prop="configure">
        <el-radio-group v-model="form.configure">
          <el-radio :label="false">装车时填写</el-radio>
          <el-radio :label="true">过磅时填写</el-radio>
        </el-radio-group>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getDriverConfig as getConfig, setDriverConfig as setConfig } from '@/api/config/mes/base'
import { ref, computed, onMounted, inject } from 'vue'
import { ElRadioGroup } from 'element-plus'

import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  configure: undefined
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
    const { configure = false } = await getConfig()
    form.value = { configure }
    dataSource.value = { configure }
  } catch (error) {
    console.log('获取物流信息填写配置', error)
  } finally {
    dataLoading.value = false
  }
}

async function submit() {
  submitLoading.value = true
  try {
    await setConfig(form.value)
    ElNotification({
      title: '物流信息配置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
  } catch (error) {
    console.log('设置物流信息配置', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
