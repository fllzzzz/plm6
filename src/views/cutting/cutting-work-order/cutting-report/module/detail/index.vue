<template>
  <common-dialog
    title="零件详情"
    width="70%"
    :show-close="false"
    :close-on-click-modal="false"
    v-model="drawerVisible"
    :before-close="handleClose"
  >
    <template #titleRight>
      <common-button size="mini" @click="handleClose">关 闭</common-button>
    </template>

    <div class="flex-rss">
      <common-table
        :summary-method="getSummaries"
        show-summary
        row-key="id"
        ref="tableRef"
        :max-height="500"
        style="width: 100%"
        :data="plateData"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="projectName" prop="projectName" :show-overflow-tooltip="true" label="所属项目" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.monomerName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="单元" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="producedQuantity" prop="producedQuantity" :show-overflow-tooltip="true" label="产量" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.producedQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重(kg)" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.netWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重(kg)" min-width="55">
          <template v-slot="scope">
            <span>{{ scope.row.totalNetWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="weight" prop="weight" :show-overflow-tooltip="true" label="完成日期" min-width="80">
          <template v-slot="scope">
            <span>{{ scope.row.weight }}</span>
          </template>
        </el-table-column>
      </common-table>
    </div>
  </common-dialog>
</template>

<script setup>
import useVisible from '@compos/use-visible'

import { defineProps, defineEmits, ref } from 'vue'
import { getPartListByMac } from '@/api/cutting/machine-part'
import { tableSummary } from '@/utils/el-extra'

const props = defineProps({
  visible: {
    type: Boolean,
    required: true
  },
  detailData: {
    type: Object
  }
})

const emit = defineEmits(['update:visible'])
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook, showHook })

const plateData = ref([]) // 页面数据

function showHook() {
  if (props.detailData) {
    plateDataGet()
  }
}

function closeHook() {
}

async function plateDataGet() {
  try {
    const content = await getPartListByMac(props.detailData)
    plateData.value = content
    console.log(' plateData.value', plateData.value)
  } catch (err) {
    console.log('钢板清单页面接口报错', err)
  }
}

function getSummaries(param) {
  return tableSummary(param, { props: ['producedQuantity', 'totalNetWeight'] })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid rgb(81, 113, 131);
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
.TaskPackage {
  margin-top: 30px;
}
.title-style {
  font-weight: 700;
  font-size: 18px;
  color: #000;
}
</style>

