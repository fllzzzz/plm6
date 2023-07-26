<template>
  <common-table
    v-loading="tableLoading"
    ref="table"
    :data="tableData"
    empty-text="暂无数据"
    :show-empty-symbol="false"
    :data-format="dataFormat"
    :max-height="maxHeight"
    style="width: 100%"
    show-summary
    :summary-method="getSummaries"
  >
    <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
    <el-table-column :show-overflow-tooltip="true" prop="project" key="project.shortName" label="项目" min-width="120" />
    <el-table-column :show-overflow-tooltip="true" prop="monomerName" key="monomerName" label="单体" min-width="100" />
    <el-table-column :show-overflow-tooltip="true" prop="serialNumber" key="serialNumber" label="编号" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="specification" key="specification" label="规格" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="material" key="material" label="材质" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="quantity" key="quantity" label="数量" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="netWeight" key="netWeight" :label="`单净重\n（kg）`" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="grossWeight" key="grossWeight" :label="`单毛重\n（kg）`" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="totalNetWeight" key="totalNetWeight" :label="`总净重\n（kg）`" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="totalGrossWeight" key="totalGrossWeight" :label="`总毛重\n（kg）`" align="center" />
    <el-table-column :show-overflow-tooltip="true" prop="picturePath" key="picturePath" label="图形" align="center" width="150">
      <template v-slot="scope">
        <!-- <div v-if="scope.row.picturePath" style="width: 100%; height: 80px">
          <el-image style="width: 100%; height: 100%" :src="scope.row.picturePath" fit="scale-down"></el-image>
        </div>
        <div v-else>未导入DXF</div> -->
        <el-image style="width: 100%; height: 100%" :src="scope.row.picturePath" fit="scale-down" @error="scope.row.imgLoad = false">
          <template #error>
            <div class="error-slot">
              <span v-if="scope.row.picturePath">加载失败</span>
              <span v-else>未导入DXF</span>
            </div>
          </template>
        </el-image>
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'

defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  tableLoading: {
    type: Boolean,
    default: false
  },
  maxHeight: {
    type: [String, Number]
  }
})

const dataFormat = ref([['project', 'parse-project']])

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity', 'totalNetWeight', 'totalGrossWeight']
  })
}

</script>

<style lang="scss" scoped>
.error-slot {
  display: flex;
  justify-content: center;
  align-items: center;
  width: 100%;
  height: 100%;
  background: #f5f7fa;
  color: #c0c4cc;
  font-size: 14px;
}
</style>
