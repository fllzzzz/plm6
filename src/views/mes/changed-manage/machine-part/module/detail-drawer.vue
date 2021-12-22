<template>
  <common-drawer ref="drawerRef" :title="`${info.serialNumber}零件详情`" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="40%">
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span> 可修改的零件数量总和为{{ info.canHandleTotalMete }}，请谨慎操作！</span>
      </div>
      <common-table ref="tableRef" v-loading="tableLoading" :data="list" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="productionLine.name" :show-overflow-tooltip="true" label="生产线" min-width="150px">
          <template v-slot="scope">
            <span>{{ scope.row.productionLine?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="taskQuantity" :show-overflow-tooltip="true" label="任务数" align="center" width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="completeQuantity" :show-overflow-tooltip="true" label="已生产" align="center" width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { taskList } from '@/api/mes/changed-manage/common'
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
    const { content } = await taskList({
      productType: props.info?.productType,
      productId: props.info?.productId
    })
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
