<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">调拨审核配置</span>
        <common-tip-button
          v-permission="permission.transferEdit"
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
    <el-form v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="left" label-width="130px">
      <el-form-item label="审核方式">
        <common-radio v-model="form.transferType" :options="returnAuditEnum.ENUM" type="enum" size="small" />
      </el-form-item>
      <el-form-item label-width="0">
        <span class="form-item-tip">办理调拨时选择通过方式，默认提交需要审核，可配置提交即通过</span>
        <span class="form-item-tip">审核后通过: 物料调拨提交后需审核通过则视为调拨成功:</span>
        <span class="form-item-tip">提交即通过:物料调拨无需审核处理，提交后即视为调拨成功:</span>
        <span class="form-item-tip">注意: 甲供调拨(包含借用、买入、归还、归还甲方) 审核方式固定为“审核后通过”不受该配置影响。</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getTransferBasicConf, setTransferBasicConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { returnAuditEnum } from '@enum-ms/wms'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import useRefreshStore from '@/composables/store/use-refresh-store'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 审核方式
  transferType: undefined
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
    const res = await getTransferBasicConf()
    form.value = deepClone(res)
    dataSource.value = res
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

    await setTransferBasicConf(form.value)
    ElNotification({
      title: '调拨审核配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    useRefreshStore('wmsConfig')
  } catch (error) {
    ElNotification({
      title: '调拨审核配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
