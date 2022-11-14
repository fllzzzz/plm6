<template>
  <common-drawer
    ref="drawerRef"
    :title="`产线：${detailData.workShopName}>${detailData.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1200"
  >
    <template #titleRight>
      <print-table
        api-key="mesProductionLineList"
        :params="{
          productionLineId: props.detailData.id,
          productType: props.detailData.productType,
          startTime: props.detailData.startDate,
          endTime: props.detailData.endDate,
        }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </template>
    <template #content>
      <!--表格渲染-->
      <common-table ref="tableRef" :max-height="maxHeight" :data="productionLineData" return-source-data style="width: 100%">
        <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="180">
          <template v-slot="scope">
            <span>{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" key="monomer.name" label="单体" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="area.name" key="area.name" label="区域" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column
          :show-overflow-tooltip="true"
          prop="specification"
          key="specification"
          label="规格"
          min-width="120"
          align="center"
        ></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="任务数" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="weight" key="weight" label="单重" align="center"></el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" key="completeQuantity" label="完成数" align="center">
          <template #default="{ row }">
            <span v-if="row.status === workOrderTypeEnum.NORMAL.V">{{ row.completeQuantity }}</span>
            <span style="color: red" v-else>{{ row.completeQuantity }}</span>
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
import { productionLineDetail } from '@/api/mes/task-tracking/production-line-tracking.js'
import { workOrderTypeEnum } from '@enum-ms/mes'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, defineEmits, ref } from 'vue'
import { projectNameFormatter } from '@/utils/project'

const emit = defineEmits(['update:visible'])
const productionLineData = ref([])
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

const { maxHeight } = useMaxHeight({
  paginate: true
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: productionLineDetailGet })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: productionLineDetailGet })

async function productionLineDetailGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await productionLineDetail({
      productionLineId: props.detailData.id,
      taskType: props.detailData.productType,
      startTime: props.detailData.startDate,
      endTime: props.detailData.endDate,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取产线跟踪详情失败', e)
  } finally {
    productionLineData.value = _list
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

