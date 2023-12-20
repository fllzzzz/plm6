<template>
  <div>
    <div v-show="!props.processData.id">
      <div class="my-code">点击左边表项目行查看详情</div>
    </div>
    <div v-show="props.processData.id">
      <div class="head-container">
        <common-radio-button type="enum" v-model="weightStatus" :options="[weightTypeEnum.NET, weightTypeEnum.GROSS]" class="filter-item" />
        <monomer-select-area-select
          v-model:monomerId="monomerId"
          v-model:areaId="areaId"
          needConvert
          clearable
          areaClearable
          :project-id="props.processData.id"
        />
        <production-line-select
          v-model="productionLineId"
          :factory-id="factoryId"
          :workshop-id="workshopId"
          :clearable="true"
          class="filter-item"
          style="width: 200px"
        />
        <el-date-picker
          v-model="date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          unlink-panels
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px; margin-right: 10px"
          class="filter-item date-item"
          @change="handleDateChange"
        />
        <!-- <common-radio-button
          v-model="productType"
          :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE, componentTypeEnum.MACHINE_PART]"
          showOptionAll
          type="enum"
          size="small"
          class="filter-item"
        /> -->
      </div>
      <common-table
        ref="tableRef"
        v-loading="tableLoading"
        :data="processList"
        :empty-text="'暂无数据'"
        :max-height="maxHeight"
        row-key="id"
        style="width: 100%"
      >
        <el-table-column prop="index" label="序号" align="center" width="60px" type="index" />
        <el-table-column align="center" key="name" prop="name" :show-overflow-tooltip="true" label="涉及工序" width="100px">
          <template v-slot="scope">
            <table-cell-tag :show="scope.row.id === -1" color="#e64242" name="特殊" />
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="需生产（件/kg）">
          <template v-slot="scope">
            <span
              >{{ scope.row.quantity }}/{{
                weightStatus === weightTypeEnum.NET.V ? scope.row.totalNetWeight : scope.row.totalGrossWeight
              }}</span
            >
          </template>
        </el-table-column>
        <el-table-column align="center" key="completeQuantity" prop="completeQuantity" :show-overflow-tooltip="true" label="完成（件/kg）">
          <template v-slot="scope">
            <span
              >{{ scope.row.completeQuantity }}/{{
                weightStatus === weightTypeEnum.NET.V ? scope.row.completeNetWeight : scope.row.completeGrossWeight
              }}</span
            >
          </template>
        </el-table-column>
        <el-table-column align="center" key="rate" prop="rate" :show-overflow-tooltip="true" label="完成率" min-width="150px">
          <template v-slot="scope">
            <el-progress
              :text-inside="true"
              :stroke-width="26"
              :percentage="Number(((scope.row.completeQuantity / scope.row.quantity) * 100).toFixed(2))"
              status="success"
            />
          </template>
        </el-table-column>
        <el-table-column v-permission="permission.detail" align="center" :show-overflow-tooltip="true" label="操作">
          <template v-slot="scope">
            <common-button type="primary" size="mini" @click.stop="showDetail(scope.row)">查看</common-button>
          </template>
        </el-table-column>
      </common-table>
      <process-detail
        v-model:visible="dialogVisible"
        :project-id="props.processData.id"
        :detail-data="detailData"
        :weightStatus="weightStatus"
        :dateQuery="dateQuery"
      />
    </div>
  </div>
</template>
<script setup>
import { ref, defineProps, watch, provide, computed } from 'vue'
import { getProcessList } from '@/api/mes/production-manage/dashboard/project-overview'
// import { componentTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import { mesProjectOverviewPM as permission } from '@/page-permission/mes'
import useMaxHeight from '@compos/use-max-height'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import productionLineSelect from '@comp-mes/production-line-select'
import processDetail from '../process-detail/index.vue'

const tableRef = ref()
const processList = ref([])
const tableLoading = ref(false)
const productType = ref()
const monomerId = ref()
const areaId = ref()
const productionLineId = ref()
const weightStatus = ref(weightTypeEnum.NET.V)
const factoryId = ref()
const workshopId = ref()
const detailData = ref([])
const dialogVisible = ref(false)
const date = ref([])
const startDate = ref()
const endDate = ref()

const props = defineProps({
  processData: {
    type: Object,
    default: () => {}
  }
})

const dateQuery = computed(() => {
  return {
    startDate: startDate.value,
    endDate: endDate.value
  }
})

watch(
  [() => props.processData?.id, () => monomerId.value, () => areaId.value, () => productionLineId.value, () => weightStatus.value],
  () => {
    processListGet()
  }
)

provide('monomerId', monomerId)
provide('areaId', areaId)
provide('productionLineId', productionLineId)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

async function processListGet() {
  if (!props.processData?.id) return
  processList.value = []
  try {
    tableLoading.value = true
    const data = await getProcessList({
      productType: productType.value,
      monomerId: monomerId.value,
      areaId: areaId.value,
      projectId: props.processData.id,
      productionLineId: productionLineId.value,
      startDate: startDate.value,
      endDate: endDate.value
    })
    processList.value = data
  } catch (e) {
    console.log('获取项目下的工序清单', e)
  } finally {
    tableLoading.value = false
  }
}

function showDetail(row) {
  dialogVisible.value = true
  detailData.value = row
}

function handleDateChange() {
  if (date.value && date.value.length > 1) {
    startDate.value = date.value[0]
    endDate.value = date.value[1]
  } else {
    startDate.value = undefined
    endDate.value = undefined
  }
  processListGet()
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
