<template>
  <div>
     <!--表格渲染-->
      <common-table
        ref="tableRef"
        :data="enclosureData"
        :empty-text="blankText"
        :max-height="maxHeight"
        return-source-data
        :showEmptySymbol="false"
        style="width: 100%"
      >
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column
          v-if="props.category!==TechnologyTypeAllEnum.BENDING.V"
          key="plateId"
          prop="plateId"
          :show-overflow-tooltip="true"
          label="版型"
        >
          <template v-slot="scope">
            <div>{{ scope.row.plate? scope.row.plate: '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="category===TechnologyTypeAllEnum.BENDING.V"
          key="unfoldedWidth"
          prop="unfoldedWidth"
          :show-overflow-tooltip="true"
          :label="`展开宽度(mm)`"
        >
          <template v-slot="scope">
            <div>{{ scope.row.unfoldedWidth? scope.row.unfoldedWidth.toFixed(DP.MES_ENCLOSURE_W__MM): '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="category!==TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V"
          key="thickness"
          prop="thickness"
          :show-overflow-tooltip="true"
          :label="`板厚(mm)`"
          align="left"
        >
          <template v-slot="scope">
            <span>{{ scope.row.thickness ? scope.row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM) : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="category!=TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V && category!=TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V"
          key="color"
          prop="color"
          :show-overflow-tooltip="true"
          label="颜色"
        >
          <template v-slot="scope">
            <div>{{ scope.row.color ? scope.row.color : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          v-if="category!==TechnologyTypeAllEnum.SANDWICH_BOARD.V && category!==TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V"
          key="brand"
          prop="brand"
          :show-overflow-tooltip="true"
          label="品牌"
        >
          <template v-slot="scope">
            <div>{{ scope.row.brand ? scope.row.brand : '-' }}</div>
          </template>
        </el-table-column>
        <el-table-column
          key="quantity"
          prop="quantity"
          :label="'数量(张)'"
          align="left"
        >
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="totalLength"
          prop="totalLength"
          :label="`总长度(m)`"
          align="left"
        >
          <template v-slot="scope">
            {{ scope.row.totalLength ? scope.row.totalLength.toFixed(DP.MES_ENCLOSURE_L__M) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          key="totalArea"
          prop="totalArea"
          :show-overflow-tooltip="true"
          :label="`总面积(㎡)`"
          align="left"
        >
          <template v-slot="scope">
            {{ scope.row.totalArea ? scope.row.totalArea.toFixed(DP.COM_AREA__M2) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          key="weight"
          prop="weight"
          :show-overflow-tooltip="true"
          label="重量(kg)"
        >
          <template v-slot="scope">
            <div>{{ scope.row.weight ? scope.row.weight.toFixed(DP.COM_WT__KG) : '-' }}</div>
          </template>
        </el-table-column>
      </common-table>
  </div>
</template>

<script setup>
import { ref, defineProps } from 'vue'
import useMaxHeight from '@compos/use-max-height'
import { DP } from '@/settings/config'
import { TechnologyTypeAllEnum } from '@enum-ms/contract'

const tableRef = ref()

const props = defineProps({
  enclosureData: {
    type: Array,
    default: () => []
  },
  category: {
    type: [String, Number],
    default: undefined
  },
  blankText: {
    type: [String],
    default: '暂无数据'
  }
})
const { maxHeight } = useMaxHeight({
  wrapperBox: '.enclosureTable',
  paginate: true,
  extraHeight: 40
})

</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
