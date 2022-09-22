<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">退货基础配置</span>
        <common-tip-button
          v-permission="permission.basicRejectEdit"
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
    <el-form v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="left" label-width="160px">
      <el-form-item label="退货页面物料金额显示">
        <el-checkbox v-model="form.materialAmountDisplayWay.application" label="退货申请列表" size="mini"></el-checkbox>
        <el-checkbox v-model="form.materialAmountDisplayWay.review" label="退货审核列表" size="mini"></el-checkbox>
        <el-form-item>
          <span class="form-item-tip">勾选后，会在对应的业务场景展示物料金额。</span>
        </el-form-item>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getRejectBasicConf, setRejectBasicConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import useRefreshStore from '@/composables/store/use-refresh-store'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 物料金额显示方式
  materialAmountDisplayWay: {
    application: undefined,
    review: undefined
  }
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
    const res = await getRejectBasicConf()
    form.value = deepClone(res)
    dataSource.value = res
  } catch (error) {
    console.log('wms退货配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    submitLoading.value = true

    await setRejectBasicConf(form.value)
    ElNotification({
      title: '退货基础配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    // fetchData()
    useRefreshStore('wmsConfig')
  } catch (error) {
    ElNotification({
      title: '退货基础配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
