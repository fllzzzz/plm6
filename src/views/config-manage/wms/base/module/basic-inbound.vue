<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">基础入库配置</span>
        <common-button
          v-permission="permission.basicInboundEdit"
          :loading="submitLoading"
          :disabled="submitDisabled"
          show-tip
          size="mini"
          style="float: right"
          @click="submit"
        >
          保存
        </common-button>
      </div>
    </template>
    <el-form v-loading="dataLoading" :disabled="dataLoading" :model="form" label-position="left" label-width="120px">
      <el-form-item label="金额填写场景">
        <common-radio v-model:value="form.amountFillWay" :options="inboundFillWayEnum" type="enum" :size="'small'" />
      </el-form-item>
      <el-form-item label="工厂填写场景">
        <common-radio v-model:value="form.factoryFillWay" :options="inboundFillWayEnum" type="enum" :size="'small'" />
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { ElNotification } from 'element-plus'

import { getInboundBasicConf, setInboundBasicConf } from '@/api/config/wms/base'

import { reactive, ref, onMounted, inject, computed } from 'vue'
import { inboundFillWayEnum } from '@enum-ms/wms'
import { isObjectValueEqual } from '@/utils/data-type/object'

const permission = inject('permission')

// 数据源
let dataSource = {
  // 金额填写方式
  amountFillWay: undefined,
  // 工厂填写方式
  factoryFillWay: undefined
}
// 表单
const form = reactive(dataSource)

// loading
const dataLoading = ref(false)
const submitLoading = ref(false)

// 未修改时，禁止点击保存按钮
const submitDisabled = computed(() => isObjectValueEqual(form, dataSource))

onMounted(() => {
  fetchData()
})

// 加载数据
async function fetchData() {
  dataLoading.value = true
  try {
    const { amountFillWay, factoryFillWay } = await getInboundBasicConf()
    form.amountFillWay = amountFillWay
    form.factoryFillWay = factoryFillWay

    dataSource = { amountFillWay, factoryFillWay }
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

    await setInboundBasicConf(form)
    ElNotification({
      title: '入库基础配置设置成功',
      type: 'success',
      duration: 2500
    })
    fetchData()
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
