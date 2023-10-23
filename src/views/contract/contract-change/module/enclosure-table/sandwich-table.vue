<template>
  <!-- 夹芯板表格 -->
  <common-table
    :data="tableArr"
    return-source-data
    :showEmptySymbol="false"
    :cell-style="handleSandwichCellStyle"
  >
    <el-table-column :label="'序号'" type="index" align="center" width="50" />
     <el-table-column prop="typeName" label="变更类型" type="index" align="center" width="60">
      <template v-slot="scope">
        <span :style="`color:${scope.row.color}`">{{scope.row.typeName}}</span>
      </template>
    </el-table-column>
    <el-table-column prop="plateType" :show-overflow-tooltip="true" align="center" label="板型">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.plateType,scope.row.originVal.plateType)">
            <cell-change-preview :old="scope.row.originVal.plateType" :new="scope.row.plateType" />
          </template>
          <template v-else>
            <span>{{ scope.row.plateType }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" :show-overflow-tooltip="true" align="center" :label="`厚度\n(mm)`">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.thickness,scope.row.originVal.thickness)">
            <cell-change-preview :old="Number(scope.row.originVal.thickness).toFixed(DP.MES_ENCLOSURE_T__MM)" :new="Number(scope.row.thickness).toFixed(DP.MES_ENCLOSURE_T__MM)" />
          </template>
          <template v-else>
            <span>{{ scope.row.thickness? Number(scope.row.thickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column prop="effectiveWidth" :show-overflow-tooltip="true" align="center" :label="`有效宽度\n(mm)`">
      <template v-slot="scope">
        <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
          <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.effectiveWidth,scope.row.originVal.effectiveWidth)">
            <cell-change-preview :old="Number(scope.row.originVal.effectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM)" :new="Number(scope.row.effectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM)" />
          </template>
          <template v-else>
            <span>{{ scope.row.effectiveWidth? Number(scope.row.effectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): '' }}</span>
          </template>
        </span>
      </template>
    </el-table-column>
    <el-table-column align="center" label="钢板信息">
      <el-table-column align="center" label="内外板">
        <template v-slot="scope">
          <div class="sandwich-cell-top" :key="scope.$index">外板</div>
          <div class="sandwich-cell-bottom" :key="scope.$index">内板</div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="板形状" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outPlateShape,scope.row.originVal.outPlateShape)">
                <cell-change-preview :old="scope.row.originVal.outPlateShape" :new="scope.row.outPlateShape" />
              </template>
              <template v-else>
                <span>{{ scope.row.outPlateShape }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intPlateShape,scope.row.originVal.intPlateShape)">
                <cell-change-preview :old="scope.row.originVal.intPlateShape" :new="scope.row.intPlateShape" />
              </template>
              <template v-else>
                <span>{{ scope.row.intPlateShape }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="材质" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outMaterial,scope.row.originVal.outMaterial)">
                <cell-change-preview :old="scope.row.originVal.outMaterial" :new="scope.row.outMaterial" />
              </template>
              <template v-else>
                <span>{{ scope.row.outMaterial }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intMaterial,scope.row.originVal.intMaterial)">
                <cell-change-preview :old="scope.row.originVal.intMaterial" :new="scope.row.intMaterial" />
              </template>
              <template v-else>
                <span>{{ scope.row.intMaterial }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="品牌" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outSteelPlateBrand,scope.row.originVal.outSteelPlateBrand)">
                <cell-change-preview :old="scope.row.originVal.outSteelPlateBrand" :new="scope.row.outSteelPlateBrand" />
              </template>
              <template v-else>
                <span>{{ scope.row.outSteelPlateBrand }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom" min-width="130">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intSteelPlateBrand,scope.row.originVal.intSteelPlateBrand)">
                <cell-change-preview :old="scope.row.originVal.intSteelPlateBrand" :new="scope.row.intSteelPlateBrand" />
              </template>
              <template v-else>
                <span>{{ scope.row.intSteelPlateBrand }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="厚度(mm)" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
             <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outThickness,scope.row.originVal.outThickness)">
                <cell-change-preview :old="scope.row.originVal.outThickness?Number(scope.row.originVal.outThickness).toFixed(DP.MES_ENCLOSURE_T__MM):''" :new="scope.row.outThickness?Number(scope.row.outThickness).toFixed(DP.MES_ENCLOSURE_T__MM): ''" />
              </template>
              <template v-else>
                <span>{{ scope.row.outThickness? Number(scope.row.outThickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intThickness,scope.row.originVal.intThickness)">
                <cell-change-preview :old="scope.row.originVal.intThickness?Number(scope.row.originVal.intThickness).toFixed(DP.MES_ENCLOSURE_T__MM):''" :new="scope.row.intThickness?Number(scope.row.intThickness).toFixed(DP.MES_ENCLOSURE_T__MM): ''" />
              </template>
              <template v-else>
                <span>{{ scope.row.intThickness? Number(scope.row.intThickness).toFixed(DP.MES_ENCLOSURE_T__MM): '' }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="宽度" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outEffectiveWidth,scope.row.originVal.outEffectiveWidth)">
                <cell-change-preview :old="scope.row.originVal.outEffectiveWidth?Number(scope.row.originVal.outEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM):''" :new="scope.row.outEffectiveWidth?Number(scope.row.outEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): ''" />
              </template>
              <template v-else>
                <span>{{ scope.row.outEffectiveWidth? Number(scope.row.outEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): '' }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intEffectiveWidth,scope.row.originVal.intEffectiveWidth)">
                <cell-change-preview :old="scope.row.originVal.intEffectiveWidth?Number(scope.row.originVal.intEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM):''" :new="scope.row.intEffectiveWidth?Number(scope.row.intEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): ''" />
              </template>
              <template v-else>
                <span>{{ scope.row.intEffectiveWidth? Number(scope.row.intEffectiveWidth).toFixed(DP.MES_ENCLOSURE_W__MM): '' }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="镀层" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outPlating,scope.row.originVal.outPlating)">
                <cell-change-preview :old="scope.row.originVal.outPlating" :new="scope.row.outPlating" />
              </template>
              <template v-else>
                <span>{{ scope.row.outPlating }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intPlating,scope.row.originVal.intPlating)">
                <cell-change-preview :old="scope.row.originVal.intPlating" :new="scope.row.intPlating" />
              </template>
              <template v-else>
                <span>{{ scope.row.intPlating }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="涂层" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outCoating,scope.row.originVal.outCoating)">
                <cell-change-preview :old="scope.row.originVal.outCoating" :new="scope.row.outCoating" />
              </template>
              <template v-else>
                <span>{{ scope.row.outCoating }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intCoating,scope.row.originVal.intCoating)">
                <cell-change-preview :old="scope.row.originVal.intCoating" :new="scope.row.intCoating" />
              </template>
              <template v-else>
                <span>{{ scope.row.intCoating }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
      <el-table-column align="left" label="颜色" min-width="130" :show-overflow-tooltip="true">
        <template v-slot="scope">
          <div class="sandwich-cell-top">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.outColour,scope.row.originVal.outColour)">
                <cell-change-preview :old="scope.row.originVal.outColour" :new="scope.row.outColour" />
              </template>
              <template v-else>
                <span>{{ scope.row.outColour }}</span>
              </template>
            </span>
          </div>
          <div class="sandwich-cell-bottom">
            <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
              <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.intColour,scope.row.originVal.intColour)">
                <cell-change-preview :old="scope.row.originVal.intColour" :new="scope.row.intColour" />
              </template>
              <template v-else>
                <span>{{ scope.row.intColour }}</span>
              </template>
            </span>
          </div>
        </template>
      </el-table-column>
    </el-table-column>
    <el-table-column align="center" label="芯材信息">
      <el-table-column prop="coreBrand" :show-overflow-tooltip="true" align="center" label="品牌">
        <template v-slot="scope">
          <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
            <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.coreBrand,scope.row.originVal.coreBrand)">
              <cell-change-preview :old="scope.row.originVal.coreBrand" :new="scope.row.coreBrand" />
            </template>
            <template v-else>
              <span>{{ scope.row.coreBrand }}</span>
            </template>
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="typeName" :show-overflow-tooltip="true" align="center" label="种类">
        <template v-slot="scope">
          <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
            <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.typeName,scope.row.originVal.typeName)">
              <cell-change-preview :old="scope.row.originVal.typeName" :new="scope.row.typeName" />
            </template>
            <template v-else>
              <span>{{ scope.row.typeName }}</span>
            </template>
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="unitWeight" :show-overflow-tooltip="true" align="center" :label="`容重\n(kg/m³)`">
        <template v-slot="scope">
          <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
            <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.unitWeight,scope.row.originVal.unitWeight)">
              <cell-change-preview :old="scope.row.originVal.unitWeight" :new="scope.row.unitWeight" />
            </template>
            <template v-else>
              <span>{{ scope.row.unitWeight }}</span>
            </template>
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" :show-overflow-tooltip="true" align="center" label="数量(m)">
        <template v-slot="scope">
          <span :style="`color:${!isNotBlank(scope.row.originVal)?scope.row.color:''};`">
            <template v-if="isNotBlank(scope.row.originVal) && !judgeSameValue(scope.row.quantity,scope.row.originVal.quantity)">
              <cell-change-preview :old="scope.row.originVal.quantity" :new="scope.row.quantity" />
            </template>
            <template v-else>
              <span>{{ scope.row.quantity }}</span>
            </template>
          </span>
        </template>
      </el-table-column>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, computed } from 'vue'

import { judgeSameValue } from '@/views/contract/info/judgeSameValue'
import { isNotBlank } from '@data-type/index'
import { DP } from '@/settings/config'

import cellChangePreview from '@comp-common/cell-change-preview'

const props = defineProps({
  tableData: {
    type: Array,
    default: () => []
  },
  originData: {
    type: Array,
    default: () => []
  }
})

function handleSandwichCellStyle({ row, column, rowIndex, columnIndex }) {
  if (columnIndex >= 5 && columnIndex <= 12) {
    return 'padding:0px;'
  }
}

const tableArr = computed(() => {
  const arr = []
  props.tableData?.forEach(v => {
    if (isNotBlank(props.originData)) {
      if (props.originData.findIndex(k => k.id === v.id) > -1) {
        const findVal = props.originData.find(k => k.id === v.id)
        if (judgeSameValue(v, findVal)) {
          arr.push({
            ...v,
            typeName: '无变更',
            color: '#909399'
          })
        } else {
          arr.push({
            ...v,
            originVal: findVal,
            typeName: '修改',
            color: '#e6a23c'
          })
        }
      } else {
        arr.push({
          ...v,
          typeName: '新增',
          color: 'green'
        })
      }
    } else {
      arr.push({
        ...v,
        typeName: '新增',
        color: 'green'
      })
    }
  })
  props.originData.forEach(v => {
    if (arr.findIndex(k => k.id === v.id) < 0) {
      arr.push({
        ...v,
        typeName: '删除',
        color: 'red'
      })
    }
  })
  return arr
})

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
