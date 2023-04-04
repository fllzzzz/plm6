<template>
  <common-drawer
    ref="drawerRef"
    custom-class="production-line-tracking-drawer"
    :title="`产线：${detailData.workShopName}>${detailData.name}`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="100%"
  >
    <template #titleAfter>
      <project-cascader v-model="projectId" clearable class="filter-item" style="width: 300px" />
      <monomer-select-area-select v-model:monomerId="monomerId" v-model:areaId="areaId" needConvert clearable :project-id="projectId" />
      <el-input
        v-model.trim="serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="handleProductionLineChange"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <print-table
      v-permission="permission.print"
        api-key="mesProductionLineList"
        :params="{
          productionLineId: props.detailData.id,
          taskType: props.detailData.productType,
          startTime: props.detailData.startDate,
          endTime: props.detailData.endDate,
          projectId: projectId,
          monomerId: monomerId,
          areaId: areaId,
          serialNumber: serialNumber,
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
        <el-table-column :show-overflow-tooltip="true" prop="netWeight" key="netWeight" label="单净重" align="center"></el-table-column>
        <el-table-column v-if="props.detailData.productType !== componentTypeEnum.ASSEMBLE.V" :show-overflow-tooltip="true" prop="grossWeight" key="grossWeight" label="单毛重" align="center"></el-table-column>
        <!-- <el-table-column v-if="props.detailData.productType !== componentTypeEnum.ASSEMBLE.V" :show-overflow-tooltip="true" prop="totalNetWeight" key="totalNetWeight" label="总净重" align="center"></el-table-column>
        <el-table-column v-if="props.detailData.productType !== componentTypeEnum.ASSEMBLE.V" :show-overflow-tooltip="true" prop="totalGrossWeight" key="totalGrossWeight" label="总毛重" align="center"></el-table-column> -->
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
import { componentTypeEnum } from '@enum-ms/mes'
import { defineProps, defineEmits, ref, watch } from 'vue'
import { projectNameFormatter } from '@/utils/project'
import { mesProductionLineTrackingPM as permission } from '@/page-permission/mes'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import projectCascader from '@comp-base/project-cascader.vue'

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

watch(
  () => projectId.value,
  (val) => {
    productionLineDetailGet()
  }
)
watch(
  () => monomerId.value,
  (val) => {
    productionLineDetailGet()
  }
)
watch(
  () => areaId.value,
  (val) => {
    productionLineDetailGet()
  }
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: productionLineDetailGet })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: productionLineDetailGet })

// 高度
const { maxHeight } = useMaxHeight({
  mainBox: '.production-line-tracking-drawer',
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

// 搜索
function searchQuery() {
  productionLineDetailGet()
}
// 重置
function resetQuery() {
  monomerId.value = undefined
  areaId.value = undefined
  projectId.value = undefined
  serialNumber.value = undefined
  productionLineDetailGet()
}

function handleProductionLineChange() {
  productionLineDetailGet()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

