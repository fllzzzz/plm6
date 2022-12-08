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
          @change="handleMonomerAreaChange"
        />
        <common-radio-button
          v-model="productType"
          :options="[componentTypeEnum.ARTIFACT, componentTypeEnum.ASSEMBLE, componentTypeEnum.MACHINE_PART]"
          showOptionAll
          type="enum"
          size="small"
          class="filter-item"
          @change="handleProductTypeChange"
        />
      </div>
      <common-table ref="tableRef" :data="processList" :empty-text="'暂无数据'" :max-height="maxHeight" row-key="id" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" width="60px" type="index" />
        <el-table-column align="center" key="name" prop="name" :show-overflow-tooltip="true" label="涉及工序" width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.name }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="清单数（件）">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column align="center" key="mete" prop="mete" :show-overflow-tooltip="true" label="清单量（kg）">
          <template v-slot="scope">
            <span>{{ scope.row.mete }}</span>
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
              :percentage="((scope.row.completeQuantity / scope.row.quantity) * 100).toFixed(2)"
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
import { getProcessList } from '@/api/bridge/bridge-production-manage/project-overview'
import { componentTypeEnum } from '@enum-ms/mes'
import { bridgeProjectOverviewPM as permission } from '@/page-permission/bridge'
import useMaxHeight from '@compos/use-max-height'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import processDetail from '../process-detail/index.vue'

const tableRef = ref()
const processList = ref([])
const productType = ref()
const monomerId = ref()
const areaId = ref()
const detailData = ref([])
const dialogVisible = ref(false)

const props = defineProps({
  processData: {
    type: Object,
    default: () => {}
  }
})

watch(
  () => props.processData.id,
  (val) => {
    if (val) {
      processListGet()
    }
  }
)

provide('monomerId', monomerId)
provide('areaId', areaId)

const { maxHeight } = useMaxHeight({
  extraBox: ['.head-container'],
  paginate: true
})

async function processListGet() {
  try {
    const data = await getProcessList({
      productType: productType.value,
      monomerId: monomerId.value,
      areaId: areaId.value,
      projectId: props.processData.id
    })
    processList.value = data
  } catch (e) {
    console.log('获取项目下的工序清单', e)
  }
}

function handleMonomerAreaChange() {
  processListGet()
}

function handleProductTypeChange() {
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
