<template>
  <common-dialog ref="dialogRef" title="钻孔排产" v-model:visible="drillDialogVisible" :before-close="handleClose" width="400px">
    <template #titleRight>
      <common-button type="primary" size="mini" @click="submitForm(formRef)"> 确定 </common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="120px" class="demo-form">
      <el-form-item label="零件数" prop="machinePartQuantity">
        <span>{{ form.machinePartQuantity }}</span>
      </el-form-item>
      <el-form-item label="钻孔数" prop="holeQuantity">
        <span>{{ form.holeQuantity }}</span>
      </el-form-item>
      <el-form-item label="钻孔生产组" prop="drillGroupsId">
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
      <el-form-item label="钻孔日期" prop="drillAskCompleteTime">
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
import { componentTypeEnum } from '@enum-ms/mes'
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
  }
})
const emit = defineEmits(['update:visible'])
const { visible: drillDialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  fetchDrillGroups()
  console.log(props.queryParams, 'params')
}

const form = reactive({
  drillGroupsId: undefined,
  drillAskCompleteTime: undefined
})
const rules = {
  drillGroupsId: [{ required: true, message: '请选择钻孔生产班组', trigger: 'blur' }],
  drillAskCompleteTime: [{ required: true, message: '请选择钻孔日期', trigger: 'blur' }]
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
      v.needMachinePartLinkList?.forEach((o) => {
        _list.push({
          productId: v.id,
          quantity: o.quantity,
          id: o.id,
          needSchedulingMonth: o.date
        })
      })
    })
    await newSave({
      ...props.queryParams,
      linkList: _list,
      drillGroupsId: form.drillGroupsId,
      drillAskCompleteTime: form.drillAskCompleteTime
    })
    // const _data = []
    // const _content = []
    // props.detailData.map((v) => {
    //   _data.push({
    //     id: v.assembleDetailId,
    //     quantity: v.quantity
    //   })
    //   _content.push(v.productionLineTypeEnum)
    // })
    // const _list = {
    //   assembleSettingList: _data,
    //   projectId: props.projectId,
    //   monomerId: props.monomerId,
    //   length: form.length,
    //   kerfLength: form.kerfLength,
    //   productionLineTypeEnum: _content[0],
    //   typesettingTypeEnum: form.typesettingTypeEnum
    // }
    // batchId.value = await extrusionNesting(_list)
    handleClose()
  } catch (err) {
    console.log('无需套料排产失败', err)
  }
}
</script>
