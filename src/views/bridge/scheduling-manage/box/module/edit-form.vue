<template>
  <common-dialog title="重新分配" v-model="dialogVisible" width="400px" :before-close="handleClose">
    <template #titleRight>
      <common-button :loading="submitLoading" type="primary" size="mini" @click="submitIt">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="90px">
      <el-form-item label="编号">
        <span>{{ form.serialNumber }}</span>
      </el-form-item>
      <el-form-item label="规格">
        <span>{{ form.specification }}</span>
      </el-form-item>
      <el-form-item label="原生产组">
        <span>{{ form.workshop?.name }}>{{ form.productionLine?.name }}>{{ form.groups?.name }}</span>
      </el-form-item>
      <el-form-item label="现生产组" prop="curGroupsId">
        <el-cascader
          v-model="form.curGroupsId"
          :options="groupsTree"
          :props="{ value: 'id', label: 'name', children: 'children', expandTrigger: 'hover', emitPath: false }"
          filterable
          clearable
          style="width: 200px"
          placeholder="请选择现生产组"
        />
      </el-form-item>
      <el-form-item label="分配数量" prop="needSchedulingQuantity">
        <el-input-number
          v-model="form.needSchedulingQuantity"
          :step="1"
          :min="1"
          :max="form.schedulingQuantity"
          :precision="0"
          size="mini"
          controls-position="right"
          style="width: 200px"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { editRecord } from '@/api/bridge/scheduling-manage/box'
import { ElNotification } from 'element-plus'
import { defineEmits, defineProps, inject, ref, computed } from 'vue'

import useVisible from '@compos/use-visible'
import useSchedulingGroups from '@compos/bridge/scheduling/use-scheduling-groups'

const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  itemInfo: {
    type: Object,
    default: () => {}
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

const areaIdObj = inject('areaIdObj')
const productType = inject('productType')
const queryParams = computed(() => {
  return {
    productType: productType,
    structureClassId: props.itemInfo.structureClassId
  }
})
const factoryIds = computed(() => [areaIdObj.value[props.itemInfo.area?.id]?.factoryId])
const { groupsTree, groupsObj } = useSchedulingGroups({ queryParams, factoryIds, disabledIds: computed(() => [props.itemInfo?.groups?.id]) })

const formRef = ref()
const form = ref({})
const submitLoading = ref(false)

const rules = {
  curGroupsId: [{ required: true, message: '请选择现生产组', trigger: 'change' }],
  needSchedulingQuantity: [{ required: true, message: '请选择现生产组', trigger: 'change' }]
}

function showHook() {
  form.value = props.itemInfo
}

async function submitIt() {
  let valid = false
  formRef.value.validate((val) => {
    valid = val
  })
  try {
    if (valid) {
      submitLoading.value = true
      await editRecord([
        {
          id: form.value.id,
          quantity: form.value.needSchedulingQuantity,
          groupsId: form.value.curGroupsId,
          factoryId: factoryIds.value[0],
          productionLineId: groupsObj[form.value.curGroupsId]?.productionLine?.id,
          workshopId: groupsObj[form.value.curGroupsId]?.workshop?.id
        }
      ])
      handleClose()
      ElNotification({ title: '重新分配成功', type: 'success', duration: 2500 })
      emit('refresh')
    }
  } catch (error) {
    console.log('重新分配', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
