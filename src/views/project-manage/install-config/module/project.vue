<template>
  <el-card v-if="checkPermission(permission.get)" class="box-card" shadow>
    <template #header class="clearfix">
      <span class="title">填报方式</span>
      <div v-if="checkPermission(permission.edit) && changed" style="float: right">
        <common-button size="small" type="warning" @click="cancel">取消修改</common-button>
        <common-button :loading="saveLoading" size="small" type="success" @click="submit">保存</common-button>
      </div>
    </template>
    <div v-loading="loading || saveLoading">
      <el-form ref="projectFormRef" :model="form">
        <el-form-item label="填报方式" prop="reportMethod">
          <template v-if="checkPermission(permission.edit)">
            <el-radio-group v-model="form.reportMethod">
              <el-radio :label="1">APP填报 <span style="color:red;">（选择APP填报，PC端：安装管理>安装填报将无法使用）</span></el-radio>
              <el-radio :label="0">PC端填报 <span style="color:red;">（选择PC填报，APP端：劳务分包单位将无法填报）</span></el-radio>
            </el-radio-group>
          </template>
          <span v-else v-empty-text="installSetEnum?.[form.reportMethod]?.V" />
        </el-form-item>
      </el-form>
    </div>
  </el-card>
</template>

<script setup>
import { editInstall } from '@/api/project-manage/install-config'
import { ref, computed, defineProps, watch } from 'vue'
import { ElRadioGroup } from 'element-plus'

import checkPermission from '@/utils/system/check-permission'
import { isNotBlank } from '@data-type/index'
import { installSetEnum } from '@enum-ms/project'
import { useStore } from 'vuex'

import { installConfigPM } from '@/page-permission/project'
import { ElMessage } from 'element-plus'

const permission = installConfigPM.reportMethod
const defaultData = {
  reportMethod: undefined
}

const props = defineProps({
  globalProject: {
    type: Object,
    default: () => {}
  }
})

const store = useStore()
const projectFormRef = ref()
const loading = ref(false)
const saveLoading = ref(false)
const form = ref({ ...defaultData })

watch(
  () => props.globalProject.installReportMethod,
  (val) => {
    if (isNotBlank(val)) {
      form.value.reportMethod = props.globalProject?.installReportMethod
    }
  },
  { immediate: true, deep: true }
)

const changed = computed(() => {
  return !(form.value.reportMethod === props.globalProject?.installReportMethod)
})

async function submit() {
  saveLoading.value = true
  try {
    await editInstall({
      projectId: props.globalProject?.id,
      reportMethod: form.value.reportMethod
    })
    ElMessage.success('修改成功')
    handleSuccess()
  } catch (error) {
    console.log('设置填报方式', error)
  } finally {
    saveLoading.value = false
  }
}

async function handleSuccess() {
  try {
    await store.dispatch('project/fetchUserProjects')
    await store.dispatch('project/setProjectId', props.globalProject.id)
  } catch (e) {
    console.log(e)
  }
}

function cancel() {
  form.value.reportMethod = props.globalProject?.installReportMethod
}

</script>

<style lang="scss" scoped>
.title{
    height: 32px;
    line-height: 32px;
}
::v-deep(.el-radio.el-radio--small){
  display:block;
}
</style>
