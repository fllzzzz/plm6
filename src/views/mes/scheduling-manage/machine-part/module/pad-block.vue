<template>
  <common-drawer
    ref="drawerRef"
    title="标准零件列表"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="75%"
    custom-class="drawer-detail"
  >
    <template #content>
      <div class="head-container"></div>
      <common-table :data="padBlockList" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="left">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="left">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度\n(mm)`" min-width="80px" align="left">
          <template #default="{ row }">
            <span>{{ row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="thick"
          prop="thick"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`厚度\n(mm)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.thick ? scope.row.thick : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          key="material"
          prop="material"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="材质"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.material ? scope.row.material : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          key="netWeight"
          prop="netWeight"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单净重\n(kg)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          key="grossWeight"
          prop="grossWeight"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单毛重\n(kg)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.grossWeight ? scope.row.grossWeight : '-' }}
          </template>
        </el-table-column>
        <!-- <el-table-column
        key="totalNetWeight"
        prop="totalNetWeight"
        :show-overflow-tooltip="true"
        :label="`总净重\n(kg)`"
        align="left"
        min-width="95px"
      >
        <template v-slot="scope">
          {{ scope.row.totalNetWeight ? scope.row.totalNetWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column>
      <el-table-column
        key="totalGrossWeight"
        prop="totalGrossWeight"
        :show-overflow-tooltip="true"
        :label="`总毛重\n(kg)`"
        align="left"
        min-width="95px"
      >
        <template v-slot="scope">
          {{ scope.row.totalGrossWeight ? scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG) : '-' }}
        </template>
      </el-table-column> -->
        <el-table-column key="surfaceArea" prop="surfaceArea" sortable="custom" :label="`单面积\n(㎡)`" align="left" min-width="80px">
          <template v-slot="scope">
            {{ scope.row.surfaceArea ? scope.row.surfaceArea : '-' }}
          </template>
        </el-table-column>
        <el-table-column key="picturePath" prop="picturePath" label="图片" align="left" min-width="80px">
          <template v-slot="scope">
            <div class="board-box">
              <el-image :src="scope.row.picturePath" @error="scope.row.imgLoad = false">
                <template #error>
                  <div class="error-slot">
                    <span v-if="scope.row.picturePath">加载失败</span>
                    <span v-else>未导入DXF</span>
                  </div>
                </template>
              </el-image>
            </div>
          </template>
        </el-table-column>
        <el-table-column prop="usedQuantity" label="数量" align="left" width="120px" fixed="right">
          <template #default="{ row }">
              <el-input-number
                v-model="row.usedQuantity"
                :step="1"
                :min="1"
                :max="99999999"
                size="mini"
                style="width: 100%"
                controls-position="right"
              />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template v-slot="scope">
            <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="del(scope.row.id)" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
// import { DP } from '@/settings/config'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const padBlockList = ref([])
const emit = defineEmits(['update:visible'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  padBlockData: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  padBlockList.value = props.padBlockData
}

function del(id) {
  const delIndex = padBlockList.value.findIndex(v => v.id === id)
  padBlockList.value.splice(delIndex, 1)
}
// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.drawer-detail',
    extraBox: ['.el-drawer__header', '.head-container', '.remark-detail'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 80
  },
  drawerVisible
)
</script>

<style lang="scss" scoped>
.manual-pack-list.el-table .el-table__cell.is-hidden > * {
  visibility: visible;
}
.board-box {
  width: 80px;
  height: 80px;
  line-height: 80px;
  text-align: center;
  box-sizing: border-box;
  padding: 2px;
  border: 1px solid #dfe4ed;
  border-radius: 6px;
}
</style>
