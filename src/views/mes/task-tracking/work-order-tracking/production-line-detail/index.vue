<template>
  <common-drawer
    ref="drawerRef"
    :title="`${processMaterialListTypeEnum.VL[detailData.type]}详细清单`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1200"
  >
    <template #content>
      <div style="display: flex; justify-content: space-between; margin-bottom: 8px">
        <div>
          <!-- <el-tag style="cursor: pointer;" v-for="item in props.detailData.productionLineList" :key="item">产线：{{ item.workshop }}>{{ item.productionLine }}</el-tag> -->
          <common-select
              v-model="workshopInf"
              :options="props.detailData.productionLineList"
              :data-structure="{ key: 'workshop', label: 'workshop', value: 'workshop' }"
              clearable
              filterable
              type="other"
              size="small"
              class="filter-item"
              placeholder="请选择车间"
              @change="handleProductionChange"
            />
          <common-select
              v-model="line"
              :options="props.detailData.productionLineList"
              :data-structure="{ key: 'productionLine', label: 'productionLine', value: 'productionLine' }"
              clearable
              filterable
              type="other"
              size="small"
              class="filter-item"
              style="margin-left: 8px"
              placeholder="请选择产线"
              @change="handleProductionChange"
            />
        </div>
        <print-table api-key="workOrderTrackingList" :params="{ ...query }" size="mini" type="warning" class="filter-item" />
      </div>
      <!--表格渲染-->
      <common-table ref="tableRef" :data="partData" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" min-width="60" type="index" />
        <el-table-column prop="monomer" label="单体" align="center" min-width="100"></el-table-column>
        <el-table-column prop="area" label="区域" align="center" min-width="100"></el-table-column>
        <el-table-column
          prop="name"
          v-if="detailData.type === processMaterialListTypeEnum.ARTIFACT.V"
          label="名称"
          align="center"
          min-width="100"
        ></el-table-column>
        <el-table-column prop="serialNumber" label="编号" align="center" min-width="100"></el-table-column>
        <el-table-column prop="specification" label="规格" align="center" min-width="100"></el-table-column>
        <el-table-column prop="quantity" label="数量" align="center" min-width="100"></el-table-column>
        <el-table-column prop="weight" label="单重" align="center" min-width="100"></el-table-column>
        <el-table-column prop="finishQuantity" label="完成数" align="center" min-width="100"></el-table-column>
        <el-table-column prop="finishDate" label="完成日期" align="center" min-width="100">
          <template v-slot="scope">
            <span>{{ scope.row.finishDate ? parseTime(scope.row.finishDate, '{y}/{m}/{d}') : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="status" label="状态" align="center" min-width="100"></el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import useVisible from '@compos/use-visible'
import { defineProps, defineEmits, ref } from 'vue'
import { parseTime } from '@/utils/date'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import { getCutPart } from '@/api/cutting/project-data'

const emit = defineEmits(['update:visible', 'change'])
const partData = ref([])
const line = ref()
const workshopInf = ref()
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
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: CutPart })

async function CutPart() {
  partData.value = await getCutPart(props.detailData.taskId)
  if (partData.value === '没有零件') {
    partData.value = []
  }
}

function handleProductionChange(val) {
  emit('change', val)
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

