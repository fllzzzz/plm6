<template>
  <div class="app-container">
    <div class="head-container" style="width: 300px; float: right;">
      <print-table
        v-permission="permission.printDetail"
        api-key="bridgeMachinePartList"
        :params="{ ...props.query }"
        size="mini"
        type="warning"
        class="filter-item"
      />
    </div>
    <common-table ref="tableRef" :data="machinePartData" :empty-text="'暂无数据'" :max-height="maxHeight + 20" row-key="id" style="width: 100%">
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column align="center" key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="零部件编号">
        <template v-slot="scope">
          <span>{{ scope.row.serialNumber }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="producedQuantity" prop="producedQuantity" :show-overflow-tooltip="true" label="已生产">
        <template v-slot="scope">
          <span>{{ scope.row.producedQuantity }}</span>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>
<script setup>
import { ref, defineProps, watch } from 'vue'
import { productionDetail } from '@/api/bridge/bridge-production-manage/assembly-match'
import { bridgeAssemblyMatchDashboardPM as permission } from '@/page-permission/bridge'
import useMaxHeight from '@compos/use-max-height'

const tableRef = ref()
const machinePartData = ref([])

const props = defineProps({
  query: {
    type: Object
  }
})

watch(
  () => props.query.monomerId,
  (val) => {
    if (val) {
      partDataGet()
    }
  },
  { deep: true, immediate: true }
)

async function partDataGet() {
  machinePartData.value = []
  try {
    const data = await productionDetail({
      ...props.query
    })
    machinePartData.value = data
  } catch (e) {
    console.log('获取区域下的零件生产数据失败', e)
  }
}
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.head-container'],
    clientHRepMainH: true,
    navbar: false,
    paginate: false
  },
  tableRef
)
</script>
<style lang="scss" scoped>
.app-container {
  padding-bottom: 0;
}
</style>
