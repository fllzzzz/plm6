<template>
  <common-drawer ref="drawerRef" title="套料成果" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="100%">
    <template #titleRight> </template>
    <template #content>
      <div class="wrap">
        <div class="wrap-left">
          <nesting-task-list ref="nestingTaskListRef" :maxHeight="maxHeight" @nesting-task-click="handleNestingTaskClick"/>
        </div>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, nextTick } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import nestingTaskList from './nesting-task-list.vue'

const drawerRef = ref()
const nestingTaskListRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

function showHook() {
  nextTick(() => {
    nestingTaskListRef?.value?.initTaskList()
  })
}

function handleNestingTaskClick() {

}
</script>

<style lang="scss" scoped>
.wrap {
  display: flex;
  .wrap-left {
    width: 480px;
    margin-right: 20px;
  }
  .wrap-right {
    flex: 1;
    min-width: 0;
    overflow: hidden;
  }
}
::-webkit-scrollbar {
  width: 6px;
  height: 6px;
}
::-webkit-scrollbar-thumb {
  border-radius: 6px;
}
</style>
