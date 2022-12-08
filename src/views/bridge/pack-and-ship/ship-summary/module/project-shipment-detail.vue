<template>
  <div class="detail-container">
    <div style="margin-bottom:10px;min-height:28px;" class="tag-div">
      <div style="float:left;">
        <el-tag style="margin-right:3px;">{{`项目:${props.currentRow.project.serialNumber+' '+props.currentRow.project.shortName}`}}</el-tag>
        <monomer-select
          ref="monomerSelectRef"
          v-model="query.monomerId"
          :project-id="props.currentRow.projectId"
          :default="false"
          clearable
          class="filter-item"
          @change="fetchDetail"
          @getAreaInfo="getAreaInfo"
        />
        <common-select
          v-model="query.areaId"
          :options="areaInfo"
          type="other"
          :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
          size="small"
          clearable
          placeholder="请选择区域"
          class="filter-item"
          style="width:200px;margin-left:3px;"
          @change="fetchDetail"
        />
      </div>
      <div style="float:right;">
        <print-table
          api-key="mesProjectShipDetail"
          v-permission="permission.print"
          :params="{ projectId: props.currentRow.projectId, productionLineTypeEnum: props.productionLineTypeEnum, workshopId: props.workshopId, ...query.value }"
          size="mini"
          type="warning"
        />
      </div>
    </div>
    <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight-70">
      <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" :show-overflow-tooltip="true"/>
      <el-table-column key="areaName" prop="areaName" label="区域" align="center" :show-overflow-tooltip="true"/>
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="weight" prop="weight" label="单重" align="right" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <span>{{toThousand(scope.row.weight,DP.COM_WT__KG)}}</span><span style="margin-left:3px;">kg</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" label="清单数" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="inboundQuantity" prop="inboundQuantity" label="入库数" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="cargoQuantity" prop="cargoQuantity" label="发运数" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="cargoMete" prop="cargoMete" label="发运量" align="right" :show-overflow-tooltip="true">
        <template v-slot="scope">
         <span>{{toThousand(scope.row.cargoMete,DP.COM_WT__KG)}}</span><span style="margin-left:3px;">kg</span>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { inboundDetail } from '@/api/bridge/bridge-pack-and-ship/ship-summary'

import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import usePagination from '@compos/use-pagination'

import monomerSelect from '@/components-system/plan/monomer-select'

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
  },
  permission: {
    type: Object,
    default: () => {}
  },
  productionLineTypeEnum: {
    type: Number
  },
  workshopId: {
    type: Number
  }
})

const list = ref([])
const tableLoading = ref(false)
const areaInfo = ref([])
const query = ref({
  monomerId: undefined,
  areaId: undefined
})

const { maxHeight } = useMaxHeight({ extraBox: '.tag-div', wrapperBox: ['.app-container', '.detail-container'] })
const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchDetail })

watch(
  () => props.currentRow.projectId,
  (val) => {
    if (val) {
      fetchDetail()
    }
  },
  { deep: true, immediate: true }
)

function getAreaInfo(val) {
  areaInfo.value = val || []
}

async function fetchDetail() {
  list.value = []
  if (!props.currentRow.projectId) {
    return
  }
  tableLoading.value = true
  try {
    const { content = [], totalElements } = await inboundDetail({ projectId: props.currentRow.projectId, ...query.value, productionLineTypeEnum: props.productionLineTypeEnum, workshopId: props.workshopId, ...queryPage })
    list.value = content
    setTotalPage(totalElements)
    tableLoading.value = false
  } catch (error) {
    console.log('获取详情失败', error)
    tableLoading.value = false
  }
}

</script>
