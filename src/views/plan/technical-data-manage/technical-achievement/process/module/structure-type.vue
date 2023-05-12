<template>
  <div style="padding:10px;padding-left:0;">
    <common-table ref="tableRef" :data="list" v-loading="tableLoading" :max-height="maxHeight" style="width:300px;"  @selection-change="handleSelectionChange">
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column key="structureClassName" prop="structureClassName" label="构件类型" align="center" />
      <el-table-column key="quantity" prop="quantity" label="数量(件)" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import { getStructureClass } from '@/api/plan/technical-data-manage/process'
import { ref, defineEmits, defineProps, watch } from 'vue'

const emit = defineEmits(['change'])

const props = defineProps({
  query: {
    type: Object,
    default: () => {}
  },
  maxHeight: {
    type: Number,
    default: undefined
  }
})

const list = ref([])
const tableLoading = ref(false)
const tableRef = ref()

watch(
  () => props.query,
  (val) => {
    if (val) {
      fetchList()
    }
  },
  { deep: true, immediate: true }
)

// 获取构件类型明细
async function fetchList() {
  let _list = []
  if (!props.query.projectId && !props.query.processType) {
    list.value = _list
    return
  }
  tableLoading.value = true
  try {
    const { content = [] } = await getStructureClass({ ...props.query })
    _list = content
  } catch (error) {
    console.log('构件类型明细', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

function handleSelectionChange(val) {
  emit('change', val)
}
</script>
