<template>
  <el-drawer
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
          <span class="title">
            <span>{{ props.title }}</span>
          </span>
          <span>
          <slot name="titleRight" />
          <common-button v-if="props.showClose" @click="handleClose" size="mini" type="warning" >关闭</common-button>
          </span>
        </div>
      </slot>
    </template>

    <div v-show="contentVisible">
      <slot v-if="loaded" name="content" />
    </div>
  </el-drawer>
</template>

<script setup>
import { watch, computed, defineProps, defineEmits, defineExpose, ref } from 'vue'
import { isNotBlank } from '@data-type/index'

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
  showClose: {
    type: Boolean,
    default: true
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
  },
  showDelay: {
    type: Number,
    default: 0
  },
  loadDelay: {
    type: Number,
    default: 300
  }
})

// 自定义类名
const customClass = props.customClass || '' + ' common-drawer'

const drawerVisible = ref(false)
// 内容是否显示
const contentVisible = ref(false)
// 是否已加载
const loaded = ref(false)

// 是否使用prop:visible 控制
const isVisibleProp = computed(() => isNotBlank(props.visible))

watch([() => props.visible, () => props.modelValue], ([v, mv]) => {
  drawerVisible.value = isVisibleProp.value ? v : mv
})

watch(
  () => drawerVisible.value,
  (flag) => {
    if (flag) {
      if (props.showDelay) {
        setTimeout(() => {
          contentVisible.value = true
        }, props.showDelay)
      } else {
        contentVisible.value = true
      }
    } else {
      contentVisible.value = false
    }
  }
)

watch(contentVisible, (flag) => {
  if (flag && !loaded.value) {
    if (props.loadDelay) {
      setTimeout(() => {
        loaded.value = true
      }, props.loadDelay)
    } else {
      loaded.value = true
    }
  }
})

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
  loaded,
  handleClose
})
</script>

<style lang="scss" scoped>
.common-drawer  {
  ::v-global(.el-drawer__header) {
    margin-bottom: 15px;
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

  .title {
    display: flex;
    align-items: center;
    font-weight: bold;
    font-size: 18px;
    margin-right: 15px;
    color: #000;
    position: relative;
    padding-left: 10px;
    box-sizing: border-box;

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
}
</style>
