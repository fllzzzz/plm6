<template>
  <common-drawer
    ref="drawerRef"
    custom-class="scheduling-detail-drawer"
    :title="`排产及生产明细`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #content>
      <!--表格渲染-->
      <common-table ref="tableRef" :max-height="maxHeight" :data="productionLineData" return-source-data style="width: 100%">
        <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="180">
          <template v-slot="scope">
            <span>{{ projectNameFormatter(scope.row.project) }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="monomer.name" key="monomer.name" label="单体" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="area.name" key="area.name" label="区域" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          prop="specification"
          key="specification"
          label="规格"
          min-width="120"
          align="center"
        />
        <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
        <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="排产数" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          prop="schedulingNetWeight"
          key="schedulingNetWeight"
          label="排产量（kg）"
          align="center"
        />
        <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" key="completeQuantity" label="完成数" align="center" />
        <el-table-column
          :show-overflow-tooltip="true"
          prop="completeNetWeight"
          key="completeNetWeight"
          label="完成量（kg）"
          align="center"
        />
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
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import { defineProps, defineEmits, ref } from 'vue'
import { projectNameFormatter } from '@/utils/project'
// import { mesProductionLineTrackingPM as permission } from '@/page-permission/mes'

const emit = defineEmits(['update:visible'])
const productionLineData = ref([])
const projectId = ref()
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()

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

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: productionLineDetailGet })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: productionLineDetailGet })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.scheduling-detail-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    paginate: true
  },
  drawerVisible
)

async function productionLineDetailGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await productionLineDetail({
      productionLineId: props.detailData.id,
      taskType: props.detailData.productType,
      startTime: props.detailData.startDate,
      endTime: props.detailData.endDate,
      projectId: projectId.value,
      monomerId: monomerId.value,
      areaId: areaId.value,
      serialNumber: serialNumber.value,
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

