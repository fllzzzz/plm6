<template>
  <common-drawer
    ref="drawerRef"
    :title="`${detailData.name}工序详细清单`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="80%"
  >
    <template #titleAfter>
      <monomer-select-area-select
        v-model:monomerId="monomerId"
        v-model:areaId="areaId"
        needConvert
        clearable
        areaClearable
        :project-id="props.projectId"
      />
      <el-input
        v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
        v-model.trim="name"
        size="small"
        placeholder="输入名称搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="handleChange"
      />
      <el-input
        v-model.trim="serialNumber"
        size="small"
        placeholder="输入编号搜索"
        style="width: 170px"
        class="filter-item"
        clearable
        @keyup.enter="handleChange"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <print-table
        v-permission="permission.print"
        api-key="mesWorkOrderTrackingList"
        :params="{
          processId: props.detailData.id,
          taskType: props.detailData.productType,
          orderId: props.detailData.taskOrderId,
          name: name,
          groupId: props.detailData.group?.id,
          monomerId: monomerId,
          areaId: areaId,
          serialNumber: serialNumber,
        }"
        size="mini"
        type="warning"
        class="filter-item"
        style="width: 300px"
      />
    </template>
    <template #content>
      <!--表格渲染-->
      <common-table ref="tableRef" :data="processDetailData" :max-height="maxHeight + 50" style="width: 100%">
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
import { defineProps, defineEmits, ref, watch } from 'vue'
import { mesWorkOrderTrackingPM as permission } from '@/page-permission/mes'
import { parseTime } from '@/utils/date'
import { componentTypeEnum, workOrderTypeEnum } from '@enum-ms/mes'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'

const emit = defineEmits(['update:visible'])
const processDetailData = ref([])
const monomerId = ref()
const areaId = ref()
const name = ref()
const serialNumber = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  projectId: {
    type: Number
  }
})
watch(
  () => monomerId.value,
  (val) => {
    processDetailGet()
  }
)
watch(
  () => areaId.value,
  (val) => {
    processDetailGet()
  }
)
watch(
  () => props.detailData.id,
  (val) => {
    monomerId.value = undefined
    areaId.value = undefined
    name.value = undefined
    serialNumber.value = undefined
    processDetailGet()
  }
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: processDetailGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: processDetailGet })

const { maxHeight } = useMaxHeight({
  mainBox: '.common-drawer',
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  clientHRepMainH: true,
  extraHeight: 50,
  minHeight: 300,
  paginate: true
})

async function processDetailGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await processDetail({
      processId: props.detailData.id,
      taskType: props.detailData.productType,
      orderId: props.detailData.taskOrderId,
      groupId: props.detailData.group?.id,
      name: name.value,
      monomerId: monomerId.value,
      areaId: areaId.value,
      serialNumber: serialNumber.value,
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

// 搜索
function searchQuery() {
  processDetailGet()
}
// 重置
function resetQuery() {
  monomerId.value = undefined
  areaId.value = undefined
  name.value = undefined
  serialNumber.value = undefined
  processDetailGet()
}
function handleChange() {
  processDetailGet()
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

