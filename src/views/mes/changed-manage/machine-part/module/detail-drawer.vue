<template>
  <common-drawer
    ref="drawerRef"
    title="零件详情"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="40%"
  >
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span>
          可操作的零件数量总和为{{
            info.canHandleTotalMete
          }}，请谨慎操作！</span
        >
      </div>
      <common-table ref="tableRef" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="零件编号">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="lineName" :show-overflow-tooltip="true" label="生产线">
          <template v-slot="scope">
            <span>{{ scope.row.lineName }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="productionMete" :show-overflow-tooltip="true" label="已生产">
          <template v-slot="scope">
            <span>{{ scope.row.productionMete }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { extraTaskList } from '@/api/mes/changed-manage/artifact'
import { defineProps, defineEmits, ref, watch } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const tableRef = ref()
const drawerRef = ref()
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
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
const tableLoading = ref(false)
const list = ref([])

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
    } else {
      init()
    }
  },
  { immediate: true }
)

function init() {
  list.value = []
}

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content } = await extraTaskList()
    _list = content
  } catch (error) {
    console.log('获取处理列表失败')
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}
</script>

<style lang="scss" scoped>
.tip {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  color: red;
  font-size: 13px;
  margin-bottom: 15px;
  line-height: 20px;
  > span {
    display: inline-block;
  }
  > span:nth-child(1) {
    width: 50px;
    flex-shrink: 0;
  }
}
</style>
