<template>
  <common-dialog
    ref="dialogRef"
    title="钻孔排产"
    v-model:visible="drillDialogVisible"
    :show-close="false"
    :before-close="handleClose"
    width="400px"
  >
    <template #titleRight>
      <common-button type="primary" size="mini" @click.stop="submitForm(formRef)"> 确定 </common-button>
      <common-button size="mini" @click.stop="closed"> 关闭 </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" class="demo-form">
      <el-form-item label="零件数:" prop="quantity">
        <span>{{ drillData.quantity }}</span>
      </el-form-item>
      <el-form-item label="钻孔数:" prop="drillQuantity">
        <span>{{ drillData.drillQuantity }}</span>
      </el-form-item>
      <el-form-item label="钻孔生产组:" prop="drillGroupsId">
        <el-cascader
          v-model="form.drillGroupsId"
          :options="schedulingGroups.list"
          :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
          :show-all-levels="false"
          style="width: 100%"
          filterable
          clearable
          placeholder="请选择钻孔生产组"
        />
      </el-form-item>
      <el-form-item label="钻孔日期:" prop="drillAskCompleteTime">
        <el-date-picker
          v-model="form.drillAskCompleteTime"
          type="date"
          size="small"
          class="date-item filter-item"
          style="width: 100% !important"
          placeholder="选择排产日期"
          :clearable="false"
          format="YYYY-MM-DD"
          value-format="x"
          :disabled-date="disabledDate"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, defineEmits, reactive } from 'vue'
import { newSave } from '@/api/mes/scheduling-manage/machine-part'
import { manualFetchGroupsTree } from '@compos/mes/scheduling/use-drill-scheduling-groups'
import { componentTypeEnum, nestingTypeEnum } from '@enum-ms/mes'
import { ElNotification } from 'element-plus'
import useVisible from '@compos/use-visible'

const formRef = ref()
const dialogRef = ref()
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  totalList: {
    type: Array,
    default: () => []
  },
  queryParams: {
    type: Object
  },
  drillData: {
    type: Object,
    default: () => {}
  }
})
const emit = defineEmits(['update:visible', 'success'])
const { visible: drillDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  form.drillGroupsId = undefined
  form.drillAskCompleteTime = undefined
  fetchDrillGroups()
}

function disabledDate(time) {
  return time < new Date()
}
const form = reactive({
  drillGroupsId: undefined,
  drillAskCompleteTime: undefined
})
const rules = {
  drillGroupsId: [{ required: true, message: '请选择钻孔生产班组', trigger: 'blur' }],
  drillAskCompleteTime: [{ required: true, message: '请选择钻孔日期', trigger: 'blur' }]
}

function closed() {
  ElNotification({
    title: '本次套料作废',
    type: 'info',
    duration: 2500
  })
  handleClose()
}

// --------------------------- 获取钻孔生产班组 start ------------------------------
const groupLoad = ref(false)
const schedulingGroups = ref({ list: [], obj: {}})

async function fetchDrillGroups() {
  if (groupLoad.value) return
  try {
    schedulingGroups.value = await manualFetchGroupsTree({ productType: componentTypeEnum.MACHINE_PART.V })
    groupLoad.value = true
  } catch (e) {
    console.log('获取钻孔生产组的信息失败', e)
  }
}
// --------------------------- 获取生产班组 end --------------------------------
async function submitForm(formRef) {
  try {
    const _list = []
    props.totalList.forEach((v) => {
      if (v.needMachinePartLinkList) {
        v.needMachinePartLinkList?.forEach((o) => {
          _list.push({
            productId: v.id,
            quantity: o.quantity,
            id: o.id,
            needSchedulingMonth: o.date
          })
        })
      } else {
        _list.push({
          productId: v.id,
          quantity: v.usedQuantity
        })
      }
    })
    await newSave({
      ...props.queryParams,
      linkList: _list,
      boolOffLine: nestingTypeEnum.NORMAL.V,
      drillGroupsId: form.drillGroupsId,
      drillAskCompleteTime: form.drillAskCompleteTime
    })
    ElNotification({
      title: '钻孔排产保存成功',
      type: 'success',
      duration: 2500
    })
    handleClose()
    emit('success')
  } catch (err) {
    console.log('无需套料排产失败', err)
  }
}
</script>
