<template>
  <common-dialog
    ref="drawerRef"
    v-model="dialogVisible"
    fullscreen
    :title="`${props.detailData.name}工序生产明细`"
    :before-close="handleClose"
    :show-close="false"
    :close-on-click-modal="false"
    top="10vh"
  >
    <template #titleRight>
      <div style="display: flex">
        <print-table
          v-permission="permission.print"
          api-key="mesProjectOverviewList"
          :params="{ ...query, processId: props.detailData.id }"
          size="mini"
          type="warning"
          class="filter-item"
        />
        <common-button size="mini" style="margin-left: 8px" @click="handleClose">关 闭</common-button>
      </div>
    </template>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      :data="processDetailData"
      :max-height="maxHeight + 115"
      :show-empty-symbol="false"
      show-summary
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" align="center">
        <template #default="{ row }">
          <span>{{ row.monomer ? row.monomer?.name : '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="area.name" label="区域" align="center">
        <template #default="{ row }">
          <span>{{ row.area ? row.area?.name : '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="length" label="长度" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" label="单净重（kg）" align="center" />
      <el-table-column
        v-if="props.detailData.productType !== componentTypeEnum.ASSEMBLE.V"
        :show-overflow-tooltip="true"
        prop="grossWeight"
        label="单毛重（kg）"
        align="center"
      />
      <el-table-column :show-overflow-tooltip="true" prop="quantity" label="需生产数" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" label="完成数" align="center">
        <template #default="{ row }">
          <el-tag style="cursor: pointer" @click="showQuantity(row)">{{ row.completeQuantity }}</el-tag>
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
  </common-dialog>
  <detail-drawer v-model:visible="drawerVisible" :query="query" :team-data="teamData" />
</template>

<script setup>
import { getProcessDetail } from '@/api/mes/production-manage/dashboard/project-overview'
import { defineProps, defineEmits, ref, watch, computed, inject } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { componentTypeEnum } from '@enum-ms/mes'
import { mesProjectOverviewPM as permission } from '@/page-permission/mes'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import detailDrawer from './detail-drawer.vue'

const emit = defineEmits(['update:visible'])
const processDetailData = ref([])
const drawerVisible = ref(false)
const teamData = ref({})

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

const monomerId = inject('monomerId')
const areaId = inject('areaId')
const productionLineId = inject('productionLineId')

const query = computed(() => {
  return {
    productType: props.detailData.productType,
    processId: props.detailData.id,
    projectId: props.projectId,
    productionLineId: productionLineId.value
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: processDetailGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: processDetailGet })

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      processDetailGet()
    }
  },
  { deep: true }
)

async function processDetailGet() {
  let _list = []
  try {
    const { content = [], totalElements } = await getProcessDetail({
      processId: props.detailData.id,
      monomerId: monomerId.value,
      areaId: areaId.value,
      ...query.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取工序的生产明细失败', e)
  } finally {
    processDetailData.value = _list
  }
}

const { maxHeight } = useMaxHeight({
  paginate: true
})

// 点击完成数显示详情
function showQuantity(row) {
  drawerVisible.value = true
  teamData.value = row
}
// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity']
  })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped></style>
