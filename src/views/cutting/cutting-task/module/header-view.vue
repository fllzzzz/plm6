<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    direction="rtl"
    size="75%"
    :title="'12'"
    :wrapper-closable="false"
    :before-close="handleClose"
  >
    <template #titleRight></template>
    <template #content>
      <div class="class-unit-config">
        <div class="query-content">
          <el-input placeholder="输入搜索" class="filter-item" style="width: 200px; margin-bottom: 15px" size="small" clearable />
        </div>
      </div>
      <div class="item-name" style="float: left">余料信息</div>
      <common-table
        @selection-change="handleSelectionChange"
        ref="tableRef"
        border
        :data="IssueData"
        :highlight-current-row="false"
        row-key="id"
      >
        <el-table-column type="selection" align="center" width="55" />
        <el-table-column key="index" type="index" label="序号" align="center" width="60" />
        <el-table-column key="projectName" prop="projectName" :show-overflow-tooltip="true" label="项目/单体" min-width="70">
          <template v-slot="scope">
            <span>{{ scope.row.projectName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="plateType" prop="plateType" :show-overflow-tooltip="true" label="物料种类" min-width="70">
          <template v-slot="scope">
            <span>{{ scope.row.plateType }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="70">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="thick" prop="thick" :show-overflow-tooltip="true" label="厚(mm)" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.thick }}</span>
          </template>
        </el-table-column>
        <el-table-column key="width" prop="width" :show-overflow-tooltip="true" label="宽(mm)" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.width }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长(mm)" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="plateState" prop="plateState" :show-overflow-tooltip="true" label="状态" min-width="60">
          <template v-slot="scope">
            <span>{{ steelPlateEnum.VL[scope.row.plateState] }}</span>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref } from 'vue'
import { get } from '@/api/cutting/project-data'
import { steelPlateEnum } from '@enum-ms/cutting'
import useVisible from '@compos/use-visible'

const emit = defineEmits(['updateChange'])

const props = defineProps({
  visible: { type: Boolean, default: false }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })
const drawerRef = ref()
const IssueData = ref([])
const changeData = ref([])

// 页面数据
async function getIssueMac() {
  try {
    const { content } = await get({ mac: props.IssueMac.mac, plateState: props.IssueMac.plateState })
    IssueData.value = content
  } catch (err) {
    console.log(err)
  }
}

function handleSelectionChange(val) {
  changeData.value = val
}

function showHook() {
  if (props.IssueMac !== '') {
    getIssueMac()
  }
}

</script>

<style lang="scss" scoped>
$default-cell-mask-color: #52f09840;
::v-deep(.mask-td) {
  .cell {
    &:after {
      background-color: $default-cell-mask-color;
    }
  }
}

.classify-name {
  padding: 0 10px;
}
::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 32px;
    padding: 0;
  }
  th .cell {
    padding: 0 10px;
  }
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }

  .cell {
    .el-input__inner {
      border: none;
    }
    .el-input-number__increase {
      border-left: none;
      margin-right: 10px;
    }
    .el-input-number__decrease {
      border-right: none;
      margin-left: 10px;
    }
  }
}

.item-name {
  padding: 8px 16px;
  background-color: #ecf8ff;
  border-radius: 4px;
  border-left: 5px solid #50bfff;
  margin: 10px 0;
  margin-left: 5px;
  width: 150px;
}
</style>
