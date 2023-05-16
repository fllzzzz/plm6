<template>
  <common-drawer ref="drawerRef" title="打印详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="50%">
    <template #titleRight> </template>
    <template #content>
      <common-table v-loading="loading" :data="recordList" :data-format="dataFormat" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" prop="userName" label="操作人" min-width="110px" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" min-width="80px" />
        <el-table-column :show-overflow-tooltip="true" prop="time" label="打印时间" min-width="300px">
          <template v-slot="scope">
            <span>{{ scope.row.createTime }}</span>
            ~
            <span>{{ scope.row.updateTime }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  taskId: {
    type: [Number, String],
    default: undefined
  },
  getEnclosureRecord: {
    type: Function,
    required: true
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

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

const dataFormat = ref([
  ['createTime', 'parse-time'],
  ['updateTime', 'parse-time']
])

const loading = ref(false)
const recordList = ref([])

watch(
  [() => props.visible, () => props.taskId],
  ([visible, taskId]) => {
    if (visible) {
      fetchRecord()
    }
  },
  { immediate: true }
)

async function fetchRecord() {
  loading.value = true
  try {
    const data = await props.getEnclosureRecord({ taskId: props.taskId })
    recordList.value = data || []
  } catch (error) {
    console.log('获取打印记录', error)
  } finally {
    loading.value = false
  }
}
</script>
