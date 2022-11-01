<template>
  <common-drawer
    ref="drawerRef"
    :title="`${detailData.name}工序详细清单`"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    :size="1200"
  >
    <template #titleRight>
      <print-table
        api-key="workOrderTrackingList"
        :params="{ ...query }"
        size="mini"
        type="warning"
        class="filter-item"
        style="width: 300px"
      />
    </template>
    <template #content>
      <!-- <div style="display: flex; justify-content: space-between; margin-bottom: 8px">
         <tag-tabs
          v-model="productionLineId"
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
      </div>  -->
      <!--表格渲染-->
      <common-table ref="tableRef" :data="processDetailData" style="width: 100%">
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column prop="monomerName" key="monomerName" label="单体" align="center"></el-table-column>
        <el-table-column prop="areaName" key="areaName" label="区域" align="center"></el-table-column>
        <el-table-column
          prop="name"
          v-if="props.detailData.productType === componentTypeEnum.ARTIFACT.V"
          label="名称"
          key="name"
          align="center"
          min-width="100"
        ></el-table-column>
        <el-table-column prop="serialNumber" key="serialNumber" label="编号" align="center"></el-table-column>
        <el-table-column prop="specification" key="specification" label="规格" align="center" min-width="120px"></el-table-column>
        <el-table-column prop="quantity" key="quantity" label="数量" align="center"></el-table-column>
        <el-table-column prop="weight" key="weight" label="单重" align="center"></el-table-column>
        <el-table-column prop="completeQuantity" key="completeQuantity" label="完成数" align="center">
          <template #default="{ row }">
            <span v-if="row.status === 1">{{ row.completeQuantity }}</span>
            <span style="color: red" v-else>{{ row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="completeDate" key="completeDate" label="完成日期" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.completeDate ? parseTime(scope.row.completeDate, '{y}-{m}-{d}') : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="status" label="状态" align="center" sortable>
          <template #default="{ row }">
            <span style="color: red" v-if="row.status === 0">滞后</span>
            <span v-else>正常</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { processDetail } from '@/api/mes/task-tracking/work-order-tracking.js'
import useVisible from '@compos/use-visible'
import tagTabs from '@comp-common/tag-tabs'
import { defineProps, defineEmits, ref, inject } from 'vue'
import { parseTime } from '@/utils/date'
import { componentTypeEnum } from '@enum-ms/mes'

const emit = defineEmits(['update:visible', 'change'])
const processDetailData = ref([])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false,
  },
  detailData: {
    type: Object,
    default: () => {},
  },
})

const productionLineId = ref()
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: processDetailGet })

async function processDetailGet() {
  try {
    const data = await processDetail({
      processId: props.detailData.id,
      productType: props.detailData.productType,
      taskProcessId: props.detailData.taskProcessId
    })
    processDetailData.value = data
  } catch (e) {
    console.log('获取工序详情失败', e)
  }
}

function tabChange(val) {
  console.log(val, 'val')
}

</script>

<style rel="stylesheet/scss" lang="scss" scoped>
</style>

