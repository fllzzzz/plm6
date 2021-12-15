<template>
  <el-drawer
    ref="drawerRef"
    v-model="drawerVisible"
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
    :direction="props.direction"
    :show-close="false"
    :size="props.size"
    :title="props.title"
    :with-header="props.withHeader"
    :modal-class="props.modalClass"
    :z-index="props.zIndex"
    @open="open"
    @opened="opened"
    @close="close"
    @closed="closed"
  >
    <!-- 避免el-drawer自带的打开聚焦功能 -->
    <el-input class="remove" />

    <template #title>
      <slot name="title">
        <div class="drawer-title">
          <span class="title-left">
            <span class="title-text">{{ props.title }}</span>
            <span class="child-mr-6" v-if="!props.contentLoading"><slot name="titleAfter" /></span>
          </span>
          <span class="title-right-content">
            <slot name="titleRight" />
            <common-button v-if="props.showClose" @click="handleClose" size="mini" :type="props.closeBtnType" plain>关 闭</common-button>
          </span>
        </div>
      </slot>
    </template>
    <slot v-if="!props.contentLoading" name="content" />
  </el-drawer>
</template>

<script setup>
import { watch, computed, defineProps, defineEmits, defineExpose, ref } from 'vue'
import { isNotBlank } from '@data-type/index'
import { ElLoading } from 'element-plus'

const emit = defineEmits(['open', 'opened', 'close', 'closed', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    default: undefined
  },
  visible: {
    // crud 使用
    type: Boolean,
    default: undefined
  },
  contentLoading: {
    type: Boolean,
    default: false
  },
  showClose: {
    type: Boolean,
    default: true
  },
  closeBtnType: {
    type: String,
    default: undefined
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
  },
  direction: {
    type: String,
    default: 'rtl'
  },
  size: {
    type: [String, Number],
    default: '30%'
  },
  title: {
    type: String,
    default: undefined
  },
  withHeader: {
    type: Boolean,
    default: true
  },
  modalClass: {
    type: String,
    default: undefined
  },
  zIndex: {
    type: Number,
    default: undefined
  },
  wrapperClosable: {
    type: Boolean,
    default: true
  }
})

// 加载
let loading
// ref
const drawerRef = ref()
// 自定义类名
const customClass = `${props.customClass || ''} common-drawer`
// 显示状态
const drawerVisible = ref(false)

// 是否使用prop:visible 控制
const isVisibleProp = computed(() => isNotBlank(props.visible))

watch([() => props.visible, () => props.modelValue], ([v, mv]) => {
  drawerVisible.value = isVisibleProp.value ? v : mv
})

watch(
  [drawerVisible, () => props.contentLoading],
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
    if (drawerRef.value) el = drawerRef.value.drawerRef
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

defineExpose({
  loaded: drawerVisible,
  handleClose
})
</script>

<style lang="scss" scoped>
.common-drawer {
  ::v-global(.el-drawer__header) {
    margin-bottom: 5px;
  }
}
.remove {
  position: absolute;
  top: -99999px;
  left: -99999px;
}

.drawer-title {
  display: flex;
  align-items: center;
  justify-content: space-between;

  .title-left {
    display: inline-flex;
    align-items: center;
    position: relative;
    padding-left: 10px;
    margin-right: 15px;
    box-sizing: border-box;
    .child-mr-6 {
      display: inline-flex;
      align-items: center;
    }
    .title-text {
      font-weight: bold;
      font-size: 18px;
      color: #000;
      margin-right: 10px;
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

  .title-right-content {
    display: inline-flex;
    align-items: center;
  }

  ::v-deep(.el-button + .el-button) {
    margin-left: 6px;
  }
}
</style>
