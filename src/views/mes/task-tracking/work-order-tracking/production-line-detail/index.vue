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
        <tag-tabs
          v-model="workshopAndProductionLine"
          class="filter-item"
          :style="'width:calc(100% - 320px)'"
          :data="summaryList"
          :itemKey="'workshopId'"
          @change="tabChange"
        >
          <template #default="{ item }">
            <span>产线：</span>
            <span>{{ item.workshopName }}</span>
            <span> > </span>
            <span>{{ item.productionLineName }}</span>
          </template>
        </tag-tabs>
        <print-table api-key="workOrderTrackingList" :params="{ ...query }" size="mini" type="warning" class="filter-item" style="width: 300px" />
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
import tagTabs from '@comp-common/tag-tabs'
import { defineProps, defineEmits, ref } from 'vue'
import { parseTime } from '@/utils/date'
import { processMaterialListTypeEnum } from '@enum-ms/mes'
import { getCutPart } from '@/api/cutting/project-data'

const emit = defineEmits(['update:visible', 'change'])
const partData = ref([])
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

const summaryList = [
  { workshopId: 1, workshopName: '一车间', productionLineName: '一线' },
  { workshopId: 2, workshopName: '一车间', productionLineName: '二线' },
  { workshopId: 3, workshopName: '二车间', productionLineName: '一线' }
]
const workshopAndProductionLine = ref()
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: CutPart })

async function CutPart() {
  partData.value = await getCutPart(props.detailData.taskId)
  if (partData.value === '没有零件') {
    partData.value = []
  }
}

function tabChange(val) {
  console.log(val, 'val')
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

