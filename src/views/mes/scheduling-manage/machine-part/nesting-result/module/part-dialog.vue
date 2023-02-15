<template>
  <common-dialog ref="dialogRef" title="零件排产" v-model:visible="partDialogVisible" :before-close="handleClose" width="400px">
    <template #titleRight>
      <common-button type="primary" size="mini" @click="submitForm(formRef)"> 确定 </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" class="demo-form">
      <el-form-item label="零件数:" prop="quantity">
        <span>{{ partList.quantity }}</span>
      </el-form-item>
      <el-form-item label="零件量:" prop="totalNetWeight">
        <span>{{ partList.totalNetWeight }}</span>
      </el-form-item>
      <el-form-item label="生产组:" prop="groupsId">
        <el-cascader
          v-model="form.groupsId"
          :options="schedulingGroups.list"
          :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
          :show-all-levels="false"
          style="width: 100%"
          filterable
          clearable
          placeholder="请选择生产组"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
  <!-- 是否需要钻孔 -->
   <!-- <drill-scheduling-dialog
    v-model:visible="drillDialogVisible"
    :queryParams="queryParams"
    :total-list="totalList"
    :drill-data="drillData"
    @success="success"
  /> -->
</template>

<script setup>
import { defineProps, ref, defineEmits, reactive } from 'vue'
import { saveNestingTask } from '@/api/mes/scheduling-manage/common'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-scheduling-groups'
import { componentTypeEnum } from '@enum-ms/mes'
import { ElNotification } from 'element-plus'
import useVisible from '@compos/use-visible'
// import drillSchedulingDialog from '../../module/drill-scheduling-dialog.vue'

const formRef = ref()
const dialogRef = ref()
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  partList: {
    type: Object,
    default: () => {}
  }
})
const emit = defineEmits(['update:visible', 'success'])
const { visible: partDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  fetchPartGroups()
}

const form = reactive({
  groupsId: undefined
})
const rules = {
  groupsId: [{ required: true, message: '请选择生产班组', trigger: 'blur' }]
}

// --------------------------- 获取生产班组 start ------------------------------
const groupLoad = ref(false)
const schedulingGroups = ref({ list: [], obj: {}})

async function fetchPartGroups() {
  if (groupLoad.value) return
  try {
    schedulingGroups.value = await manualFetchGroupsTree({ productType: componentTypeEnum.MACHINE_PART.V })
    groupLoad.value = true
  } catch (e) {
    console.log('获取生产组的信息失败', e)
  }
}
// --------------------------- 获取生产班组 end --------------------------------
async function submitForm(formRef) {
  try {
    const _list = {
      // askCompleteTime: props.partList?.askCompleteTime,
      drillAskCompleteTime: props.partList?.drillAskCompleteTime,
      drillGroupsId: props.partList?.drillGroupsId,
      groupsId: form.groupsId,
      schedulingId: props.partList?.id
    }

    const data = await saveNestingTask(_list)
    console.log(data, 'data')
    ElNotification({
      title: '零件下发保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (err) {
    console.log('零件下发保存失败', err)
  }
}
</script>
