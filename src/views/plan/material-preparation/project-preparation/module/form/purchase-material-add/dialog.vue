<template>
  <common-dialog title="新增采购物料" v-model="dialogVisible" :before-close="handleClose" :width="400">
    <template #titleRight>
      <common-button type="success" size="mini" @click="confirmAdd">确认添加</common-button>
      <common-button type="warning" size="mini" @click="resetForm">重置</common-button>
    </template>
    <component ref="formRef" :is="comp" :technology-row="technologyRow" :classify-ids="boundClassifyIds" />
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, computed, ref } from 'vue'
import { isNotBlank } from '@/utils/data-type'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { regExtra } from '@compos/use-crud'
import useVisible from '@compos/use-visible'
import { ElMessage } from 'element-plus'
import steelPlate from './form/steel-plate.vue'
import sectionSteel from './form/section-steel.vue'
import auxMaterial from './form/aux-material.vue'

const emit = defineEmits(['update:modelValue', 'success'])
const props = defineProps({
  modelValue: {
    type: Boolean,
    default: false
  },
  technologyRow: {
    type: Object,
    default: () => ({})
  }
})

const comp = computed(() => {
  switch (props.technologyRow.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelPlate
    case rawMatClsEnum.MATERIAL.V:
      return auxMaterial
    case rawMatClsEnum.GAS.V:
    default:
      return steelPlate
  }
})

const formRef = ref()

// 模态框显示
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'modelValue' })

// 获取crud实例，并将实例注册进crud
const { crud } = regExtra()

const boundClassifyIds = computed(() => {
  const steelClassifyConfICKV = crud.props.steelClassifyConfICKV // 钢材配置
  if (isNotBlank(steelClassifyConfICKV)) {
    return steelClassifyConfICKV[props.technologyRow.steelClassifyConfId] || []
  } else {
    return []
  }
})

// 确认添加
async function confirmAdd() {
  const result = await formRef.value.add()
  if (result) {
    // console.log('result, props.technologyRow', result, props.technologyRow)
    emit('success', result, props.technologyRow)
    handleClose()
    ElMessage.success('添加成功')
  }
}

// 重置
function resetForm() {
  formRef.value.resetForm()
}
</script>
