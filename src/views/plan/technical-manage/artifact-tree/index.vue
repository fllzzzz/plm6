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
        :tree-props="{ children: 'children', hasChildren: 'hasChildren' }"
        :row-class-name="handleRowClassName"
        row-key="rowKey"
        default-expand-all
        style="width: 100%"
        @sort-change="crud.handleSortChange"
        @selection-change="crud.selectionChangeHandler"
      >
        <el-table-column key="selection" type="selection" width="55" />
        <el-table-column prop="index" label="序号" align="center" width="60">
          <template v-slot="scope">
            <span v-if="scope.row.children">{{ changeIndex(scope.row) }}</span>
            <span v-else class="child">{{ changeIndex(scope.row) }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="名称"
          min-width="100px"
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
            <span>{{ scope.row.serialNumber }}</span>
            <!-- <span style="cursor: pointer;" @dblclick="drawingPreview(scope.row)">{{ scope.row.serialNumber }}</span> -->
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('specification')"
          key="specification"
          prop="specification"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="规格"
          min-width="120"
        />
        <el-table-column
          v-if="columns.visible('length')"
          key="length"
          prop="length"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`长度\n(mm)`"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('material')"
          key="material"
          prop="material"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="材质"
          align="center"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          sortable="custom"
          label="数量"
          align="left"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('netWeight')"
          key="netWeight"
          prop="netWeight"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单净重\n(kg)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('grossWeight')"
          key="grossWeight"
          prop="grossWeight"
          sortable="custom"
          :show-overflow-tooltip="true"
          :label="`单毛重\n(kg)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.grossWeight ? scope.row.grossWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('totalNetWeight')"
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
          v-if="columns.visible('totalGrossWeight')"
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
        </el-table-column>
        <el-table-column
          v-if="columns.visible('drawingNumber')"
          key="drawingNumber"
          prop="drawingNumber"
          sortable="custom"
          :show-overflow-tooltip="true"
          label="图号"
          min-width="100px"
        />
        <el-table-column
          v-if="columns.visible('surfaceArea')"
          key="surfaceArea"
          prop="surfaceArea"
          sortable="custom"
          :label="`面积\n(㎡)`"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.surfaceArea ? scope.row.surfaceArea.toFixed(DP.COM_AREA__M2) : '-' }}
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
        <el-table-column
          v-if="columns.visible('createUser')"
          key="createUser"
          prop="createUser"
          :show-overflow-tooltip="true"
          label="上传人"
          min-width="110"
        />
        <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="上传时间" min-width="160px">
          <template v-slot="scope">
            <div>
              <span v-parse-time="'{y}-{m}-{d}'">{{ scope.row.createTime }}</span>
            </div>
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" width="80px" fixed="right">
          <!-- <template slot="header">
          <el-tooltip
            class="item"
            effect="light"
            :content="`零构件进行与暂停: \n
          1.无论有无生产均可以执行暂停；\n
          2.暂停后，无法扫码上传。\n`"
            placement="top"
          >
            <div style="display:inline-block;">
              <span>状态</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template> -->
          <template v-slot="scope">
            <el-switch
              v-model="scope.row.boolStatusEnum"
              :disabled="!checkPermission(permission.edit)"
              active-color="#13ce66"
              :active-value="true"
              :inactive-value="false"
              @change="changeStatus(scope.row, scope.row.boolStatusEnum)"
            />
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" :permission="permission" :show-del="false" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm :project-id="globalProjectId" />
    </template>
    <!-- <template v-else>
      <div style="color:red;font-size:14px;">*请先前去合同管理模块添加项目内容</div>
    </template> -->
  </div>
</template>

<script setup>
import crudApi, { editStatus } from '@/api/plan/technical-manage/artifact-tree'
import { ref } from 'vue'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import { mapGetters } from '@/store/lib'
import mHeader from './module/header'
import mForm from './module/form'
import { DP } from '@/settings/config'
import { ElMessageBox } from 'element-plus'

const { globalProject, globalProjectId } = mapGetters(['globalProject', 'globalProjectId'])
// crud交由presenter持有
const permission = {
  get: ['artifact-tree:get'],
  edit: ['artifact-tree:edit'],
  del: ['artifact-tree:del'],
  importList: ['artifact-tree:import']
}

const optShow = {
  add: false,
  edit: false,
  del: true,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '零构件清单',
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
  wrapperBox: '.artifact-tree',
  paginate: true,
  extraHeight: 157
})

function changeIndex(val) {
  if (val.children) {
    return val.index
  } else {
    return val.childIndex
  }
}

function handleRowClassName({ row, rowIndex }) {
  if (row.children) {
    return 'abnormal-row'
  } else {
    return 'hidden-select'
  }
}

CRUD.HOOK.handleRefresh = (crud, data) => {
  let index = 1
  data.data.content = data.data.content.map((v) => {
    v.dataType = 1
    v.rowKey = `${v.id}`
    v.index = index
    index++
    let childIndex = 1
    if (v.machinePartDTOList && v.machinePartDTOList.length > 0) {
      v.children = v.machinePartDTOList.map((child) => {
        child.dataType = 2
        child.rowKey = `${v.id}__${child.id}`
        child.childIndex = childIndex
        childIndex++
        return child
      })
    } else {
      v.children = []
    }
    return v
  })
}

async function changeStatus(data, val) {
  try {
    const messageName = val ? '启用' : '暂停'
    await ElMessageBox.confirm('确定' + messageName + '?', '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    await editStatus(data.type, data.id)
    crud.notify(`“${data.serialNumber}”已【${messageName}】`, CRUD.NOTIFICATION_TYPE.SUCCESS)
  } catch (error) {
    console.log('修改零构件状态', error)
    data.boolStatusEnum = !data.boolStatusEnum
  }
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.projectId = globalProjectId
  return !!crud.form.projectId
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
