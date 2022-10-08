<template>
  <el-card shadow="always">
    <template #header>
      <div class="clearfix">
        <span class="card-title">报表中心配置（入库明细和出库明细）</span>
        <common-tip-button
          v-permission="permission.reportCenterEdit"
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
    <el-form v-loading="dataLoading" :disabled="formDisabled" :model="form" label-position="top" label-width="120px">
      <el-form-item label="含税价显示">
        <el-switch
          v-model="form.amountShow"
          :active-value="1"
          :inactive-value="0"
          class="drawer-switch"
        />
      </el-form-item>

      <el-form-item label="不含税价显示">
        <el-switch
          v-model="form.amountExcludingTAXShow"
          :active-value="1"
          :inactive-value="0"
          class="drawer-switch"
        />
      </el-form-item>
       <el-form-item>
        <span class="form-item-tip">选择任何一项，税率必须显示（普票除外）。</span>
      </el-form-item>
    </el-form>
  </el-card>
</template>

<script setup>
import { getReportCenterConf, setReportCenterConf } from '@/api/config/wms/base'
import { ref, onMounted, inject, computed } from 'vue'
import { deepClone } from '@/utils/data-type'
import { isObjectValueEqual } from '@data-type/object'

import useRefreshStore from '@/composables/store/use-refresh-store'
import { ElNotification } from 'element-plus'

const permission = inject('permission')

// 数据源
const dataSource = ref({
  // 显示含税金额
  amountShow: 0,
  // 显示不含税金额
  amountExcludingTAXShow: 0
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
    const res = await getReportCenterConf()
    form.value = deepClone(res)
    dataSource.value = res
  } catch (error) {
    console.log('报表中心配置', error)
  } finally {
    dataLoading.value = false
  }
}

// 保存数据
async function submit() {
  try {
    submitLoading.value = true

    await setReportCenterConf(form.value)
    ElNotification({
      title: '报表中心配置设置成功',
      type: 'success',
      duration: 2500
    })

    dataSource.value = deepClone(form.value)
    useRefreshStore('wmsConfig')
  } catch (error) {
    ElNotification({
      title: '报表中心配置设置失败',
      type: 'error',
      duration: 2500
    })
  } finally {
    submitLoading.value = false
  }
}
</script>
