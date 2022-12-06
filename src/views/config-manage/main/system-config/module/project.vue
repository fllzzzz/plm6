<template>
  <el-card v-if="checkPermission(permission.get)" class="box-card" shadow="hover">
    <template #header class="clearfix">
      <span class="title">项目信息配置</span>
      <div v-if="checkPermission(permission.edit) && changed" style="float: right">
        <common-button size="small" type="warning" @click="cancel">取消修改</common-button>
        <common-button :loading="saveLoading" size="small" type="success" @click="submit">保存</common-button>
      </div>
    </template>
    <div v-loading="loading || saveLoading">
      <el-form ref="projectFormRef" :model="form">
          <el-form-item label="是否在数据展示时显示项目全称(不勾选显示项目简称)" prop="showProjectFullName">
          <el-checkbox v-if="checkPermission(permission.edit)" v-model="form.showProjectFullName" />
          <span v-else v-empty-text="whetherEnum?.VL[form.showProjectFullName]" />
        </el-form-item>
        <el-form-item label="是否在展示项目名称时显示合同编号" prop="showSerialNumber">
          <el-checkbox v-if="checkPermission(permission.edit)" v-model="form.showSerialNumber" />
          <span v-else v-empty-text="whetherEnum?.VL[form.showSerialNumber]" />
        </el-form-item>
        <el-form-item label="项目名称展示方式" prop="arrangement">
          <template v-if="checkPermission(permission.edit)">
            <el-radio v-for="item in projectNameArrangementModeEnum.ENUM" :key="item.K" v-model="form.arrangement" :label="item.V">{{ item.L }}</el-radio>
          </template>
          <span v-else v-empty-text="projectNameArrangementModeEnum?.VL[form.arrangement]" />
        </el-form-item>
      </el-form>
    </div>
  </el-card>
</template>

<script setup>
import { getProjectConfig, setProjectConfig } from '@/api/config/main/system-config'
import { ref, computed } from 'vue'

import { useStore } from 'vuex'
import checkPermission from '@/utils/system/check-permission'
import { isObjectValueEqual } from '@data-type/object'
import { projectNameArrangementModeEnum } from '@enum-ms/contract'
import { whetherEnum } from '@enum-ms/common'

import { systemConfigPM } from '@/page-permission/config'
import { ElMessage } from 'element-plus'

const permission = systemConfigPM.project
const defaultData = {
  arrangement: void 0,
  showProjectFullName: void 0,
  showSerialNumber: void 0
}

const projectFormRef = ref()
const store = useStore()
const loading = ref(false)
const saveLoading = ref(false)
const form = ref({ ...defaultData })
const sourceData = ref({ ...defaultData })

const changed = computed(() => {
  return !isObjectValueEqual(form.value, sourceData.value)
})

fetch()

async function fetch() {
  if (!checkPermission(permission.get)) return
  loading.value = true
  try {
    const { showProjectFullName, showSerialNumber, arrangement } = await getProjectConfig() || {}
    sourceData.value = { showProjectFullName, showSerialNumber, arrangement }
    form.value = { showProjectFullName, showSerialNumber, arrangement }
  } catch (error) {
    console.log('error', error)
  } finally {
    loading.value = false
  }
}

async function submit() {
  saveLoading.value = true
  try {
    await setProjectConfig(form.value)
    sourceData.value = { ...form.value }
    ElMessage.success('修改成功')
    store.dispatch('settings/changeSetting', new Map([['projectNameShowConfig', form.value]]))
    store.dispatch('project/fetchUserProjects')
  } catch (error) {
    console.log('设置基础信息：项目信息', error)
  } finally {
    saveLoading.value = false
  }
}

function cancel() {
  form.value = { ...sourceData.value }
}

</script>

<style lang="scss" scoped>
.title{
    height: 32px;
    line-height: 32px;
}
</style>
