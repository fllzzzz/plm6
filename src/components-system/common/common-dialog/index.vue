<template>
  <el-dialog
    ref="dialogRef"
    v-model="dialogVisible"
    :title="props.title"
    :width="props.width"
    :fullscreen="props.fullscreen"
    :top="props.top"
    :center="props.center"
    :append-to-body="props.appendToBody"
    :lock-scroll="props.lockScroll"
    :before-close="handleClose"
    :close-on-click-modal="props.closeOnClickModal"
    :close-on-press-escape="props.closeOnPressEscape"
    :open-delay="props.openDelay"
    :close-delay="props.closeDelay"
    :custom-class="customClass"
    :destroy-on-close="props.destroyOnClose"
    :modal="props.modal"
    :show-close="false"
    @open="open"
    @opened="opened"
    @close="close"
    @closed="closed"
  >
    <template #title>
      <slot name="title">
        <div class="dialog-title">
          <span class="title-left">
            <span class="title-text">{{ props.title }}</span>
            <span class="child-mr-6" v-if="!props.contentLoading"><slot name="titleAfter" /></span>
          </span>
          <span class="dialog-title-right" v-if="!props.contentLoading">
            <slot name="titleRight" />
            <common-button v-if="props.showClose" @click="handleClose" size="mini" :type="props.closeBtnType" plain>关闭</common-button>
          </span>
        </div>
      </slot>
    </template>
    <slot v-if="!props.contentLoading" />
    <template v-if="slots.footer" #footer>
      <slot v-if="!props.contentLoading" name="footer" />
    </template>
  </el-dialog>
</template>

<script setup>
import { watch, computed, defineProps, defineEmits, ref, useSlots } from 'vue'
import { isNotBlank } from '@data-type/index'
import { ElLoading } from 'element-plus'

const slots = useSlots()

const emit = defineEmits(['open', 'opened', 'close', 'closed', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: undefined
  },
  // crud用
  visible: {
    // crud 使用
    type: Boolean,
    default: undefined
  },
  showClose: {
    type: Boolean,
    default: true
  },
  contentLoading: {
    type: Boolean,
    default: false
  },
  closeBtnType: {
    type: String,
    default: undefined
  },
  title: {
    type: String,
    default: undefined
  },
  top: {
    type: String,
    default: '15vh'
  },
  center: {
    type: Boolean,
    default: true
  },
  width: {
    type: [String, Number],
    default: '50%'
  },
  fullscreen: {
    type: Boolean,
    default: false
  },

  appendToBody: {
    type: Boolean,
    default: true
  },
  lockScroll: {
    type: Boolean,
    default: true
  },
  beforeClose: {
    type: Function,
    default: undefined
  },
  closeOnClickModal: {
    type: Boolean,
    default: true
  },
  closeOnPressEscape: {
    type: Boolean,
    default: true
  },
  openDelay: {
    type: Number,
    default: 0
  },
  closeDelay: {
    type: Number,
    default: 0
  },
  customClass: {
    type: String,
    default: undefined
  },
  destroyOnClose: {
    type: Boolean,
    default: false
  },
  modal: {
    type: Boolean,
    default: true
  }
})

// 加载
let loading
// ref
const dialogRef = ref()
// 自定义类名
const customClass = `${props.customClass || ''} common-dialog`
// 显示状态
const dialogVisible = ref(false)

// 是否使用prop:visible 控制
const isVisibleProp = computed(() => isNotBlank(props.visible))

watch([() => props.visible, () => props.modelValue], ([v, mv]) => {
  dialogVisible.value = isVisibleProp.value ? v : mv
})

watch(
  [dialogVisible, () => props.contentLoading],
  ([visible, ld]) => {
    if (visible && ld) {
      openLoading()
    } else {
      loading && loading.close()
    }
  },
  { immediate: true }
)

function openLoading() {
  if (loading) {
    loading.visible = true
  }
  {
    let el
    if (dialogRef.value) el = dialogRef.value.dialogRef
    loading = ElLoading.service({
      target: el,
      lock: true,
      text: '数据加载中，请稍后',
      fullscreen: false,
      background: 'rgba(255, 255, 255, 0.5)'
    })
  }
}

function handleClose() {
  if (typeof props.beforeClose === 'function') {
    props.beforeClose(hide)
  } else {
    hide()
  }
}

function hide() {
  if (!isVisibleProp.value) {
    emit('update:modelValue', false)
  }
}

function open() {
  emit('open')
}

function opened() {
  emit('opened')
}

function close() {
  emit('close')
}

function closed() {
  emit('closed')
}
</script>
<style lang="scss" scoped>
.common-dialog {
  .dialog-title {
    display: flex;
    align-items: center;
    justify-content: space-between;

    .title-left {
      display: flex;
      align-items: center;
      position: relative;
      padding-left: 10px;
      margin-right: 15px;
      box-sizing: border-box;
      .title-text {
        font-weight: bold;
        font-size: 18px;
        color: #000;
      }
      &::before {
        content: '';
        width: 4px;
        height: 15px;
        border-radius: 10px;
        background: #1890ff;
        position: absolute;
        top: 50%;
        left: 0;
        transform: translateY(-50%);
      }
    }
    ::v-deep(.el-button + .el-button) {
      margin-left: 6px;
    }
  }
}
</style>
