<template>
  <common-drawer
    ref="drawerRef"
    title="垫块列表"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="75%"
    custom-class="drawer-detail"
  >
    <template #content>
      <div class="head-container"></div>
      <common-table :data="padBlockData" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" min-width="80px" align="center">
          <template #default="{ row }">
            <span>{{ row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" min-width="80px" align="center">
          <template #default="{ row }">
            <span>{{ row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="length" :label="`长度(mm)`" min-width="80px" align="center">
          <template #default="{ row }">
            <span>{{ row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" prop="netWeight" :label="`单净重(kg)`" min-width="80px" align="center">
          <template #default="{ row }">
            <span>{{ row.netWeight }}</span>
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
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'handleSuccess'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  padBlockData: {
    type: Array,
    default: () => []
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

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

<style>
.manual-pack-list.el-table .el-table__cell.is-hidden > * {
  visibility: visible;
}
</style>
