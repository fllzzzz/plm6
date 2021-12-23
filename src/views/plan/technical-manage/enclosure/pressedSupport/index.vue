<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length > 0">
      <!--工具栏-->
      <div class="head-container">
        <mHeader :project-id="globalProjectId" />
      </div>
      <!--表格渲染-->
      <common-table
        ref="tableRef"
        v-loading="crud.loading"
        :data="crud.data"
        :empty-text="crud.emptyText"
        :max-height="maxHeight"
        style="width: 100%"
        @sort-change="crud.handleSortChange"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="名称"
          min-width="120"
        />
        <el-table-column
          v-if="columns.visible('plate')"
          key="plate"
          prop="plate"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="版型"
          min-width="120px"
        />
        <el-table-column
          v-if="columns.visible('serialNumber')"
          key="serialNumber"
          prop="serialNumber"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="140px"
        >
          <!-- <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`双击编号可预览图纸`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>编号</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template> -->
          <template v-slot="scope">
            <span style="cursor: pointer" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('material')"
          key="material"
          prop="material"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="材质"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('length')"
          key="length"
          prop="length"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单长\n(㎜)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ENCLOSURE_L__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          sortable="custom"
          label="数量(张)"
          align="left"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('thickness')"
          key="thickness"
          prop="thickness"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`板厚\n(mm)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.thickness ? scope.row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('width')"
          key="width"
          prop="width"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`有效宽度\n(mm)`"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('totalArea')"
          key="totalArea"
          prop="totalArea"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`总面积`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.totalArea ? scope.row.totalArea.toFixed(DP.COM_AREA__M2) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalLength')"
          key="totalLength"
          prop="totalLength"
          sortable="custom"
          :label="`总长度\n(m)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.totalLength ? scope.row.totalLength.toFixed(DP.MES_ENCLOSURE_L__M) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('remark')"
          key="remark"
          prop="remark"
          :show-overflow-tooltip="true"
          label="备注"
          min-width="120"
        />
        <!--状态、编辑与删除-->
        <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="70px" fixed="right">
          <template v-slot="scope">
            <el-switch
              v-model="scope.row.boolStatusEnum"
              :disabled="!checkPermission(permission.edit)"
              active-color="#13ce66"
              :active-value="processingEnum.PROCESS.V"
              :inactive-value="processingEnum.PAUSE.V"
              @change="changeStatus(scope.row, scope.row.boolStatusEnum)"
            />
          </template>
        </el-table-column>
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="180px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
    </template>
    <!-- <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template> -->
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/plan/technical-manage/enclosure'
import { ref, watch } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import { DP } from '@/settings/config'
import { processingEnum } from '@enum-ms/plan'
import mForm from './module/form'
import { ElMessageBox } from 'element-plus'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['pressedSupport:get'],
  edit: ['pressedSupport:edit'],
  del: ['pressedSupport:del'],
  editStatus: ['pressedSupport:editStatus'],
  importList: ['pressedSupport:import']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '压型楼层板清单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['areaId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)
const { maxHeight } = useMaxHeight({
  wrapperBox: '.trussSupport',
  paginate: true,
  extraHeight: 157
})

watch(
  () => globalProjectId,
  (val) => {
    if (val) {
      crud.query.projectId = globalProjectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

async function changeStatus(data, val) {
  try {
    const messageName = val === 1 ? '启用' : '暂停'
    await ElMessageBox.confirm('确定' + messageName + '?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.id)
    crud.notify(`“${data.name}”已【${messageName}】`, CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.toQuery()
  } catch (error) {
    console.log('修改桁架楼层板状态', error)
    data.boolStatusEnum = data.boolStatusEnum === 1 ? 0 : 1
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
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
