<template>
  <common-drawer
    ref="drawerRef"
    :title="`${detailData.name}工序详细清单`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1200"
  >
    <template #titleRight>
      <print-table
        api-key="mesWorkOrderTrackingList"
        :params="{
          processId: props.detailData.id,
          productType: props.detailData.productType,
          orderId: props.detailData.taskOrderId,
        }"
        size="mini"
        type="warning"
        class="filter-item"
        style="width: 300px"
      />
    </template>
    <template #content>
      <!--表格渲染-->
      <common-table ref="tableRef" :data="processDetailData" :max-height="maxHeight" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" key="monomer.name" label="单体" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="area.name" key="area.name" label="区域" align="center"></el-table-column>
        <el-table-column
          :show-overflow-tooltip="true"
          prop="name"
          v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
          label="名称"
          key="name"
          align="center"
          min-width="100"
        ></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column
          :show-overflow-tooltip="true"
          prop="specification"
          key="specification"
          label="规格"
          align="center"
          min-width="120px"
        ></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="数量" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="weight" key="weight" label="单重" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" key="completeQuantity" label="完成数" align="center">
          <template #default="{ row }">
            <span v-if="row.status === workOrderTypeEnum.NORMAL.V">{{ row.completeQuantity }}</span>
            <span style="color: red" v-else>{{ row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeDate" key="completeDate" label="完成日期" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeDate ? parseTime(scope.row.completeDate, '{y}-{m}-{d}') : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="status" label="状态" align="center" sortable>
          <template #default="{ row }">
            <span style="color: red" v-if="row.status === workOrderTypeEnum.DELAY.V">{{ workOrderTypeEnum.VL[row.status] }}</span>
            <span v-else>{{ workOrderTypeEnum.VL[row.status] }}</span>
          </template>
        </el-table-column>
      </common-table>
      <!-- 分页 -->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
    </template>
  </common-drawer>
</template>

<script setup>
import { processDetail } from '@/api/mes/task-tracking/work-order-tracking.js'
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import usePagination from '@compos/use-pagination'
import { defineProps, defineEmits, ref } from 'vue'
import { parseTime } from '@/utils/date'
import { componentTypeEnum, workOrderTypeEnum } from '@enum-ms/mes'

const emit = defineEmits(['update:visible', 'change'])
const processDetailData = ref([])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: processDetailGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: processDetailGet })

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

async function processDetailGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await processDetail({
      processId: props.detailData.id,
      taskType: props.detailData.productType,
      orderId: props.detailData.taskOrderId,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取工序详情失败', e)
  } finally {
    processDetailData.value = _list
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

