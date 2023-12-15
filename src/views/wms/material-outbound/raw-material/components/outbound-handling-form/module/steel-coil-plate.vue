<template>
  <common-dialog
    ref="dialogRef"
    title="条板转换办理"
    v-model="dialogVisible"
    :width="'95%'"
    :before-close="handleClose"
    :show-close="true"
    custom-class="wms-outbound-handling"
    :top="'5vh'"
  >
    <template #titleRight>
      <span class="step-btn">
        <common-button size="mini" plain :disabled="step === 0" @click="step--">上一步</common-button>
        <common-button size="mini" plain :disabled="step === stepOptions.length - 1" @click="handleNextStep">下一步</common-button>
        <common-button :loading="submitLoading" size="mini" type="warning" :disabled="step !== stepOptions.length - 1" @click="submit">
          确认提交
        </common-button>
      </span>
      <!-- <common-button :loading="submitLoading" size="mini" type="primary" @click="submit"> 提 交 </common-button> -->
    </template>
    <component ref="componentRef" :is="currentView" :basic-class="props.basicClass" :material="props.material" :max-height="maxHeight" />
  </common-dialog>
</template>

<script setup>
import { defineEmits, defineProps, provide, computed, ref, nextTick, reactive } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import steelCoilPlateForm from './steel-coil-plate-form.vue'
import steelCoilPreview from './steel-coil-preview.vue'
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

// const outboundFormRef = ref()
// const submitLoading = ref(false)
const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const { outboundCfg } = useWmsConfig()
provide('outboundCfg', outboundCfg)

const dialogRef = ref()
const componentRef = ref()
const stepOptions = reactive([{ title: '条板处理' }, { title: '预览' }])
const stepComponent = [steelCoilPlateForm, steelCoilPreview]
const step = ref(0)
const submitLoading = ref(false)

const currentView = computed(() => stepComponent[step.value])

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
  dialogVisible
)

async function handleNextStep() {
  if (componentRef.value?.validateSubmit) {
    const validate = await componentRef.value.validateSubmit()
    if (!validate) return
  }
  step.value++
}

// // 重置表单
// function resetForm() {
//   outboundFormRef.value && outboundFormRef.value.resetForm()
// }

// // 清空校验
// function clearValidate() {
//   outboundFormRef.value && outboundFormRef.value.clearValidate()
// }

// 显示钩子
function showHook() {
  // nextTick(() => {
  //   clearValidate()
  // })
}

// // 表单提交
async function submit() {
  // try {
  //   submitLoading.value = true
  //   await outboundFormRef.value.submit()
  //   ElMessage.success('已加入出库清单')
  //   emit('success')
  //   handleClose()
  //   resetForm()
  // } catch (error) {
  //   console.log('出库办理', error)
  // } finally {
  //   submitLoading.value = false
  // }
}
</script>
