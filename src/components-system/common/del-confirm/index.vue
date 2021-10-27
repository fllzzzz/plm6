<template>
  <div class="del-confirm">
    <el-dialog title="提示" v-model="dialogVisible" :width="props.width" :before-close="handleClose" :show-close="false" :top="top">
      <span class="tip"><i class="el-icon-warning" />{{ props.tip }}</span>
      <template #footer>
        <span class="dialog-footer">
          <common-button v-if="showExit" type="danger" size="small" @click="handleExit">退 出</common-button>
          <common-button size="small" @click="handleCancel">取 消</common-button>
          <common-button :loading="subLoading" type="primary" size="small" @click="handleSubimt">确 定</common-button>
        </span>
      </template>
    </el-dialog>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref, computed, watch, nextTick } from 'vue'

const emit = defineEmits(['success', 'error', 'exit', 'cancel', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  fn: {
    type: Function,
    require: true
  },
  showExit: {
    type: Boolean,
    default: false
  },
  width: {
    type: String,
    default: '400px'
  },
  top: {
    type: String,
    default: '33vh'
  },
  tip: {
    type: String,
    default: '此操作将删除该记录, 是否继续?'
  }
})

const subLoading = ref(false)

const dialogVisible = computed(() => props.modelValue)

watch(dialogVisible, (val) => {
  if (!val) {
    nextTick(() => {
      subLoading.value = false
    })
  }
})

/**
 * 确认删除
 */
async function handleSubimt() {
  if (subLoading.value) return
  subLoading.value = true
  try {
    await props.fn()
    handleClose()
    emit('success')
  } catch (error) {
    console.log('error', error)
  }
}

/**
 * 取消
 */
function handleCancel() {
  emit('cancel')
  handleClose()
}

/**
 * 退出
 */
function handleExit() {
  emit('exit')
  handleClose()
}

/**
 * 关闭窗口
 */
function handleClose() {
  emit('update:modelValue', false)
}
</script>

<style lang="scss" scoped>
.del-confirm {
  ::v-deep(.el-dialog__header) {
    padding: 10px 20px;
  }
  ::v-deep(.el-dialog__body) {
    padding: 10px 20px;
  }
  ::v-deep(.el-dialog__footer) {
    padding: 5px 20px 15px 20px;
  }
  .tip {
    display: inline-flex;
    align-items: center;

    .el-icon-warning {
      font-size: 22px;
      color: #ff8100;
      margin-right: 5px;
    }
  }
}
</style>
