<template>
  <div>
    <div v-show="!props.processData.id">
      <div class="my-code">点击左边表项目行查看详情</div>
    </div>
    <div v-show="props.processData.id">
      <div class="head-container">
        <monomer-select-area-select
          v-model:monomerId="monomerId"
          v-model:areaId="areaId"
          needConvert
          clearable
          :project-id="props.processData.id"
        />
        <common-radio-button
          v-model="productType"
          :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE, componentTypeEnum.MACHINE_PART]"
          showOptionAll
          type="enum"
          size="small"
          class="filter-item"
        />
        <el-date-picker
          v-model="date"
          type="daterange"
          range-separator=":"
          size="small"
          value-format="x"
          :clearable="false"
          :shortcuts="PICKER_OPTIONS_SHORTCUTS"
          unlink-panels
          start-placeholder="开始日期"
          end-placeholder="结束日期"
          style="width: 240px; margin-right: 10px"
          class="filter-item date-item"
          @change="handleDateChange"
        />
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
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="需生产数（件）">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="需生产量（kg）">
          <template v-slot="scope">
            <span>{{ scope.row.totalNetWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="completeQuantity" prop="completeQuantity" :show-overflow-tooltip="true" label="完成（件）">
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}</span>
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
      <process-detail v-model:visible="dialogVisible" :project-id="props.processData.id" :detail-data="detailData" />
    </div>
  </div>
</template>
<script setup>
import { ref, defineProps, watch, provide } from 'vue'
import { getProcessList } from '@/api/mes/production-manage/dashboard/project-overview'
import { PICKER_OPTIONS_SHORTCUTS } from '@/settings/config'
import { componentTypeEnum } from '@enum-ms/mes'
import { mesProjectOverviewPM as permission } from '@/page-permission/mes'
import useMaxHeight from '@compos/use-max-height'
import moment from 'moment'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import processDetail from '../process-detail/index.vue'

const tableRef = ref()
const processList = ref([])
const tableLoading = ref(false)
const productType = ref()
const monomerId = ref()
const areaId = ref()
const detailData = ref([])
const dialogVisible = ref(false)
const date = ref([moment().startOf('month').valueOf(), moment().valueOf()])

const startDate = ref()
const endDate = ref()

const props = defineProps({
  processData: {
    type: Object,
    default: () => {}
  }
})

watch(
  [() => props.processData?.id, () => monomerId.value, () => areaId.value, () => productType.value],
  () => {
    processListGet()
  }
)

provide('monomerId', monomerId)
provide('areaId', areaId)
provide('startDate', startDate)
provide('endDate', endDate)

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

// 时间变动
function handleDateChange(val) {
  if (val && val.length > 1) {
    startDate.value = val[0]
    endDate.value = val[1]
  } else {
    startDate.value = undefined
    endDate.value = undefined
  }
  processListGet()
}

function showDetail(row) {
  dialogVisible.value = true
  detailData.value = row
}
</script>
<style lang="scss" scoped>
.app-container {
  padding: 0;
}
</style>
