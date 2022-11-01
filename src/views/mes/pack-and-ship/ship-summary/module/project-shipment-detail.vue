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
        <!-- <print-table
          v-permission="permission.print"
          api-key="productShipmentDetail"
          :params="{ projectId: props.currentRow.projectId, ...query.value }"
          size="mini"
          type="warning"
        /> -->
        <print-table
          api-key="mesProjectShipDetail"
          :params="{ projectId: props.currentRow.projectId, ...query.value }"
          size="mini"
          type="warning"
        />
      </div>
    </div>
    <common-table :data="list" v-loading="tableLoading" :max-height="maxHeight-70">
      <el-table-column key="monomer.name" prop="monomer.name" label="单体" align="center" :show-overflow-tooltip="true"/>
      <el-table-column key="area.name" prop="area.name" label="区域" align="center" :show-overflow-tooltip="true"/>
      <el-table-column key="serialNumber" prop="serialNumber" label="编号" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="productType" prop="productType" label="类型" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <span>{{componentTypeEnum.VL[scope.row.productType]}}</span>
        </template>
      </el-table-column>
      <el-table-column key="unitMete" prop="unitMete" label="单量" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <span v-if="scope.row.productType===componentTypeEnum.ARTIFACT.V">
            <span>{{toThousand(scope.row.unitMete,DP.COM_WT__KG)}}</span>
          </span>
          <span v-if="scope.row.productType===componentTypeEnum.ENCLOSURE.V">
            <span>{{toThousand(scope.row.unitMete,DP.COM_L__M)}}</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column key="unit" prop="unit" label="单位" align="center" :show-overflow-tooltip="true" width="60">
        <template v-slot="scope">
          <span v-if="scope.row.productType===componentTypeEnum.ARTIFACT.V">
            <span style="margin-left:3px;">kg</span>
          </span>
          <span v-if="scope.row.productType===componentTypeEnum.ENCLOSURE.V">
            <span style="margin-left:3px;">m</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" label="清单数" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="warehouseQuantity" prop="warehouseQuantity" label="入库数" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="shipQuantity" prop="shipQuantity" label="发运数" align="center" :show-overflow-tooltip="true" />
      <el-table-column key="shipMete" prop="shipMete" label="发运量" align="center" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <span v-if="scope.row.productType===componentTypeEnum.ARTIFACT.V">
            <span>{{toThousand(scope.row.shipMete,DP.COM_WT__KG)}}</span>
          </span>
          <span v-if="scope.row.productType===componentTypeEnum.ENCLOSURE.V">
            <span>{{toThousand(scope.row.shipMete,DP.COM_L__M)}}</span>
          </span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import { ref, defineProps, watch } from 'vue'
import { inboundDetail } from '@/api/mes/pack-and-ship/ship-summary'

import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import { componentTypeEnum } from '@enum-ms/mes'

import monomerSelect from '@/components-system/plan/monomer-select'

const props = defineProps({
  currentRow: {
    type: Object,
    default: () => {}
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

watch(
  () => props.currentRow.projectId,
  (val) => {
    if (val) {
      // fetchDetail()
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
    const { content } = await inboundDetail({ projectId: props.currentRow.projectId, ...query.value })
    list.value = content
    tableLoading.value = false
  } catch (error) {
    console.log('获取详情失败', error)
    tableLoading.value = false
  }
}

</script>
