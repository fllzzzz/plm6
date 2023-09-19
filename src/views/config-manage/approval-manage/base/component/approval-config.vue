<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">审批配置</span>
        <common-tip-button
          v-permission="permission.approvalConfigEdit"
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
      <el-form-item label="是否开启申购审批">
        <common-radio v-model="form.requisition" :options="whetherEnum.ENUM" type="enum" size="small" />
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getApprovalConf, setApprovalConf } from '@/api/config/approval-config/base'
import { ref, onMounted, inject, computed } from 'vue'
import { whetherEnum } from '@enum-ms/common'
import { isObjectValueEqual } from '@data-type/object'
import { deepClone } from '@/utils/data-type'

import { useStore } from 'vuex'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 申购审批
  requisition: false
})
// 表单
const form = ref(dataSource.value)

const store = useStore()

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
    const res = await getApprovalConf()
    const _data = { requisition: res }
    form.value = deepClone(_data)
    dataSource.value = _data
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

    await setApprovalConf(Number(form.value.requisition))
    ElNotification({
      title: '审批配置设置成功',
      type: 'success',
      duration: 2500
    })
    dataSource.value = deepClone(form.value)
    // fetchData()
    store.dispatch('config/fetchApprovalCfg')
  } catch (error) {
    ElNotification({
      title: '审批配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
