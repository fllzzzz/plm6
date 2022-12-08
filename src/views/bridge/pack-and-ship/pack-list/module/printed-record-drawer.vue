<template>
  <common-drawer ref="drawerRef" title="打印详情" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="50%">
    <template #content>
      <common-table ref="table" v-loading="loading" :data="recordList" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" prop="userName" label="操作人" min-width="110px" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" label="数量" align="center" min-width="80px" />
        <el-table-column :show-overflow-tooltip="true" prop="time" label="打印时间" min-width="300px">
          <template v-slot="scope"> {{ parseTime(scope.row.startTime) }} ~ {{ parseTime(scope.row.endTime) }} </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { getForPackage as getPrintRecord } from '@/api/bridge/label-print/print-record'
import { ref, defineEmits, defineProps, watch } from 'vue'
import { parseTime } from '@/utils/date'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  packageId: {
    type: [Number, String],
    default: undefined
  }
})
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300
  },
  () => drawerRef.value.loaded
)

const recordList = ref([])
const loading = ref(false)

watch(
  () => drawerVisible.value,
  (val) => {
    if (val) {
      fetchRecord()
    }
  },
  { immediate: true }
)

async function fetchRecord() {
  if (!props.packageId) {
    return
  }
  loading.value = true
  let _recordList = []
  try {
    const { content = [] } = await getPrintRecord(props.packageId)
    _recordList = content
  } catch (error) {
    console.log('获取打印记录', error)
  } finally {
    recordList.value = _recordList
    loading.value = false
  }
}
</script>
