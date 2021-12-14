<template>
  <!-- 夹芯板表格 -->
  <common-table
    :data="tableData"
    :cell-style="handleSandwichCellStyle"
  >
    <el-table-column :label="'序号'" type="index" align="center" width="60" />
    <el-table-column prop="brand" :show-overflow-tooltip="true" align="center" label="品名">
      <template v-slot="scope">
        <span>{{ scope.row.brand }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="板型">
      <template v-slot="scope">
        <span>{{ scope.row.plateType }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" :show-overflow-tooltip="true" align="center" label="厚度">
      <template v-slot="scope">
        <span>{{ scope.row.thickness? Number(scope.row.thickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="effectiveWidth" :show-overflow-tooltip="true" align="center" label="有效宽度">
      <template v-slot="scope">
        <span>{{ scope.row.effectiveWidth? Number(scope.row.effectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): '' }}</span>
      </template>
    </el-table-column>
    <el-table-column align="center" label="钢板信息">
      <el-table-column align="center" label="内外板">
        <template>
          <div class="sandwich-cell-top">外板</div>
          <div class="sandwich-cell-bottom">内板</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="板形状">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outPlateShape }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intPlateShape }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="材质">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outMaterial }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intMaterial | emptyTextFormatter }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="品牌">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outSteelPlateBrand | emptyTextFormatter }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intSteelPlateBrand | emptyTextFormatter }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="厚度">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outThickness? Number(scope.row.outThickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intThickness? Number(scope.row.intThickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="宽度">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outEffectiveWidth? Number(scope.row.outEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): '' }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intEffectiveWidth? Number(scope.row.intEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): '' }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="镀层">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outPlating }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intPlating }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="涂层">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outCoating }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intCoating }}</div>
        </template>
      </el-table-column>
      <el-table-column align="center" label="颜色">
        <template v-slot="scope">
          <div class="sandwich-cell-top">{{ scope.row.outColour }}</div>
          <div class="sandwich-cell-bottom">{{ scope.row.intColour }}</div>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column align="center" label="芯材信息">
      <el-table-column prop="coreBrand" :show-overflow-tooltip="true" align="center" label="品牌">
        <template v-slot="scope">
          <span>{{ scope.row.coreBrand }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="typeName" :show-overflow-tooltip="true" align="center" label="种类">
        <template v-slot="scope">
          <span>{{ scope.row.typeName }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="unitWeight" :show-overflow-tooltip="true" align="center" label="容重">
        <template v-slot="scope">
          <span>{{ scope.row.unitWeight? Number(scope.row.unitWeight).toFixed(2): '' }}</span>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column v-if="!isShow" label="操作" align="center" width="110" fixed="right">
      <template v-slot="scope">
        <common-button size="small" class="el-icon-edit" type="primary" @click="editRow(scope.$index,scope.row)" />
        <common-button size="small" class="el-icon-delete" type="danger" @click="deleteRow(scope.$index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { ref } from 'vue'
import { DP } from '@/settings/config'
const props = defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  isShow: {
    type: Boolean,
    default: true
  }
})

const emit = defineEmits(['edit'])

function  handleSandwichCellStyle({ row, column, rowIndex, columnIndex }) {
  if (columnIndex >= 5 && columnIndex <= 12) {
    return 'padding:0px;'
  }
}

function deleteRow (index){
  props.tableData.splice(index, 1)
}

function editRow(index, row) {
  emit('edit', row)
  props.tableData.splice(index, 1)
}
</script>

<style lang='scss' scoped>
.sandwich-cell-top{
  border-bottom: 1px solid #dfe6ec;
  padding: 10px;
}
.sandwich-cell-bottom{
  padding: 10px;
}
</style>