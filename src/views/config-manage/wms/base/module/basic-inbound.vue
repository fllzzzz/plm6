<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">基础入库配置</span>
        <common-tip-button
          v-permission="permission.basicInboundEdit"
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
    <el-form v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="left" label-width="120px">
      <el-form-item label="金额填写场景">
        <common-radio v-model="form.amountFillWay" :options="inboundFillWayEnum.ENUM" type="enum" size="small" />
      </el-form-item>
      <el-form-item label="工厂填写场景">
        <common-radio v-model="form.factoryFillWay" :options="inboundFillWayEnum.ENUM" type="enum" size="small" />
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getInboundBasicConf, setInboundBasicConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { inboundFillWayEnum } from '@enum-ms/wms'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 金额填写方式
  amountFillWay: undefined,
  // 工厂填写方式
  factoryFillWay: undefined
})
// 表单
const form = ref(dataSource.value)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form.value, dataSource.value))
const formDisabled = computed(() => dataLoading.value || submitLoading.value)

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const { amountFillWay, factoryFillWay } = await getInboundBasicConf()
    form.value.amountFillWay = amountFillWay
    form.value.factoryFillWay = factoryFillWay

    dataSource.value = { amountFillWay, factoryFillWay }
  } catch (error) {
    console.log('wms基础配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    submitLoading.value = true

    await setInboundBasicConf(form.value)
    ElNotification({
      title: '入库基础配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    // fetchData()
    // TODO:更新配置
    // await store.dispatch('config/fetchConfigInfo')
  } catch (error) {
    ElNotification({
      title: '基础入库配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
