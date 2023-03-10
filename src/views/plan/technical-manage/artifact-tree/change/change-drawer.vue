<template>
  <common-drawer
    ref="drawerRef"
    title="清单变更"
    v-model="drawerVisible"
    direction="rtl"
    :showClose="false"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleRight>
      <common-button @click="handleClose" size="mini" plain>取消本次变更</common-button>
    </template>
    <template #content>
      <el-card class="step-content">
        <common-step v-model="step" :options="stepOptions" space="33%" finish-status="success" />
        <span class="step-btn">
          <common-button size="mini" plain :disabled="step === 0" @click="step--">上一步</common-button>
          <common-button size="mini" plain :disabled="step === stepOptions.length - 1 || true" @click="step++">下一步</common-button>
          <common-button size="mini" type="warning" :disabled="step !== stepOptions.length - 1">确认提交</common-button>
        </span>
      </el-card>
      <component :is="currentView" :height-style="heightStyle" />
    </template>
  </common-drawer>
</template>

<script setup>
import { deepClone } from '@/utils/data-type'
import { defineProps, defineEmits, computed, ref, reactive, provide } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import commonStep from '@comp-common/common-step/index'
import mHandle from './module/handle'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  originChangeInfo: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight, heightStyle } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.step-content'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    extraHeight: 50
  },
  drawerVisible
)

const stepOptions = reactive([{ title: '变更处理' }, { title: '变更汇总' }, { title: '技术成果上传' }, { title: '变更原因填写' }])
const stepComponent = [mHandle]
const step = ref(0)
const changeInfo = ref([])
provide('changeInfo', changeInfo)
provide('originChangeInfo', props.originChangeInfo)

const currentView = computed(() => stepComponent[step.value])

function showHook() {
  changeInfo.value = deepClone(props.originChangeInfo)
}
</script>

<style lang="scss" scoped>

.step-content {
  position: relative;
  margin-bottom: 10px;

  ::v-deep(.el-card__body) {
    padding: 15px;
  }

  .step-btn {
    position: absolute;
    top: 50%;
    right: 15px;
    transform: translateY(-50%);
  }
}
</style>
