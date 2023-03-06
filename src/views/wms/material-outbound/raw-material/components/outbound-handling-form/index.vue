<template>
  <common-dialog
    title="出库办理"
    v-model="dialogVisible"
    :width="!enlargeWth ? '830px' : '90%'"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-outbound-handling"
    :top="!enlargeWth ? '10vh' : '5vh'"
  >
    <template #titleRight>
      <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button>
    </template>
    <component ref="outboundFormRef" :is="comp" :basic-class="props.basicClass" :material="props.material" :max-height="maxHeight" />
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, provide, computed, ref, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import steelPlate from './module/steel-plate.vue'
import sectionSteel from './module/section-steel.vue'
import steelCoil from './module/steel-coil.vue'
import auxMat from './module/aux-mat.vue'
import gas from './module/gas.vue'
import useWmsConfig from '@/composables/store/use-wms-config'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['success', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    // 基础分类
    type: Number
  },
  material: {
    // 物料信息
    type: Object
  }
})

const comp = computed(() => {
  switch (props.basicClass) {
    case rawMatClsEnum.STEEL_PLATE.V:
      return steelPlate
    case rawMatClsEnum.SECTION_STEEL.V:
      return sectionSteel
    case rawMatClsEnum.STEEL_COIL.V:
      return steelCoil
    case rawMatClsEnum.MATERIAL.V:
      return auxMat
    case rawMatClsEnum.GAS.V:
      return gas
    default:
      return auxMat
  }
})

const outboundFormRef = ref()
const submitLoading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { outboundCfg } = useWmsConfig()
provide('outboundCfg', outboundCfg)

const enlargeWth = computed(() => outboundFormRef.value?.enlargeWth)

// 表格最大高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.wms-outbound-handling',
    extraBox: [
      '.el-dialog__header',
      '.plate-out-material-info',
      '.material-outbound-mode-info',
      '.form-info',
      '.other-info'
    ],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false,
    minHeight: 300,
    extraHeight: 200
  },
  enlargeWth
)

// 重置表单
function resetForm() {
  outboundFormRef.value && outboundFormRef.value.resetForm()
}

// 清空校验
function clearValidate() {
  outboundFormRef.value && outboundFormRef.value.clearValidate()
}

// 显示钩子
function showHook() {
  nextTick(() => {
    clearValidate()
  })
}

// 表单提交
async function submit() {
  try {
    submitLoading.value = true
    await outboundFormRef.value.submit()
    ElMessage.success('已加入出库清单')
    emit('success')
    handleClose()
    resetForm()
  } catch (error) {
    console.log('出库办理', error)
  } finally {
    submitLoading.value = false
  }
}
</script>
