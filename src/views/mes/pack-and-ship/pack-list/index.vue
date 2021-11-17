<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader ref="headerRef" />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :cell-class-name="handelCellClassName"
      style="width: 100%"
      @selection-change="crud.selectionChangeHandler"
    >
      <el-table-column type="selection" width="55" align="center" />
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="!crud.query.projectId && columns.visible('project')"
        key="project"
        prop="project"
        :show-overflow-tooltip="true"
        label="项目"
        min-width="250"
      >
        <template v-slot="scope">
          <span class="project-name">{{ projectNameFormatter(scope.row.project) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('packageNumber')"
        key="packageNumber"
        prop="packageNumber"
        :show-overflow-tooltip="true"
        label="包单号"
        align="center"
        min-width="140px"
      />
      <el-table-column
        v-if="columns.visible('productType')"
        key="productType"
        prop="productType"
        label="产品类型"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <el-tag style="margin-right: 5px" :type="packTypeEnum[packTypeEnum.VK[scope.row.productType]].T" effect="light">{{
            packTypeEnum.VL[scope.row.productType]
          }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('packerName')"
        key="packerName"
        prop="packerName"
        :show-overflow-tooltip="true"
        label="打包办理人"
        align="center"
        min-width="120"
      />
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="日期" width="160px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('quantity')"
        key="quantity"
        prop="quantity"
        :show-overflow-tooltip="true"
        label="数量"
        align="center"
        min-width="60"
      />
      <el-table-column
        v-if="columns.visible('totalGrossWeight') && crud.query.productType === packTypeEnum.STRUCTURE.V"
        key="totalGrossWeight"
        prop="totalGrossWeight"
        :show-overflow-tooltip="true"
        label="结构毛重(kg)"
        align="right"
        min-width="120"
      >
        <template v-slot="scope">
          {{ toFixed(scope.row.totalGrossWeight, DP.COM_WT__KG) }}
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('enclLength') && crud.query.productType === packTypeEnum.ENCLOSURE.V"
        key="enclLength"
        prop="enclLength"
        :show-overflow-tooltip="true"
        label="围护长度(m)"
        align="right"
        min-width="120"
      >
        <template v-slot="scope">
          {{ convertUnits(scope.row.enclLength, 'mm', 'm', DP.MES_ENCLOSURE_L__M) }}
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('status')" key="status" prop="status" label="状态" align="center" min-width="100">
        <template v-slot="scope">
          <el-tag :type="packStatusTypeEnum[packStatusTypeEnum.VK[scope.row.status]].T">{{ packStatusTypeEnum.VL[scope.row.status] }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('remark')"
        key="remark"
        prop="remark"
        :show-overflow-tooltip="true"
        label="备注"
        min-width="140"
      />
      <!--包单操作-->
      <el-table-column
        v-if="checkPermission([...permission.download, ...permission.detail])"
        label="包单"
        width="70px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button type="primary" icon="el-icon-document" size="mini" @click.stop="showDetail(scope.row)" />
        </template>
      </el-table-column>
      <!--标签操作-->
      <el-table-column
        v-if="checkPermission([...permission.print, ...permission.printRecords])"
        label="标签"
        width="170px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button v-permission="permission.print" icon="el-icon-printer" type="success" size="mini" @click="printLabel(scope.row)" />
          <common-button v-permission="permission.print" icon="el-icon-view" type="primary" size="mini" @click="previewLabel(scope.row)" />
          <common-button
            v-permission="permission.printRecords"
            icon="el-icon-time"
            type="info"
            size="mini"
            @click="openRecordView(scope.row)"
          />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        v-if="checkPermission([...permission.detail, ...permission.edit, ...permission.del])"
        label="操作"
        min-width="120px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <template v-if="scope.row.status === packStatusTypeEnum.UNENTRUCK.V">
            <el-popconfirm title="确定要编辑此条打包记录吗？" @confirm="edit(scope.row.id, scope.row.project.id)">
              <template #reference>
                <common-button v-permission="permission.edit" type="primary" icon="el-icon-edit" size="mini" />
              </template>
            </el-popconfirm>
            <el-popconfirm title="确定要删除此条打包记录吗？" @confirm="del(scope.row.id)">
              <template #reference>
                <common-button v-permission="permission.del" type="danger" icon="el-icon-delete" size="mini" />
              </template>
            </el-popconfirm>
          </template>
        </template>
      </el-table-column>
      <!-- 详情 -->
    </common-table>
    <!--分页组件-->
    <pagination />
    <label-dlg v-model:visible="labelVisible" :label-data="currentLabel" />
    <m-detail v-model:visible="detailVisible" :package-info="packageInfo" :weight-type="weightTypeEnum.GROSS.V" />
    <printed-record-drawer v-model:visible="recordVisible" :package-id="printedRecordId" />
  </div>
</template>

<script setup>
import crudApi, { detail } from '@/api/mes/pack-and-ship/pack-list'
import { ref } from 'vue'
import { useRouter } from 'vue-router'
import { ElNotification } from 'element-plus'

import { packTypeEnum, packStatusTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum } from '@enum-ms/common'
import checkPermission from '@/utils/system/check-permission'
import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'
import { projectNameFormatter } from '@/utils/project'
import { convertUnits } from '@/utils/convert/unit'
import { parseTime } from '@/utils/date'
import { debounce } from '@/utils'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'
import labelDlg from './module/label-dlg'
import printedRecordDrawer from './module/printed-record-drawer'

const router = useRouter()

// crud交由presenter持有
const permission = {
  get: ['mesPack:get'],
  download: ['mesPack:download'],
  detail: ['mesPack:detail'],
  edit: ['mesPack:edit'],
  del: ['mesPack:del'],
  print: ['mesPack:print'],
  printPackList: ['mesPack:printPackList'],
  printRecords: ['mesPack:printRecords']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const headerRef = ref()
const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '打包记录',
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    invisibleColumns: []
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

const recordVisible = ref(false)
const labelVisible = ref(false)
const detailVisible = ref(false)
const printedRecordId = ref()
const currentLabel = ref({})
const packageInfo = ref({})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.packerName = v.userName
    v.materialTypeNames = packTypeEnum.VL[v.productType]
    return v
  })
}

// 查看详情
function showDetail(row) {
  packageInfo.value = row
  detailVisible.value = true
}

async function printLabel(row) {
  try {
    await headerRef.value.print([row])
  } catch (error) {
    console.log('打印标签失败', error)
  }
}

function previewLabel(row) {
  currentLabel.value = headerRef.value.getlabelInfo(row)
  labelVisible.value = true
}

function openRecordView(row) {
  printedRecordId.value = row.id
  recordVisible.value = true
}

function handelCellClassName({ row, column, rowIndex, columnIndex }) {
  const markColumn = ['packageNumber'] // 标记字段
  let className = ''
  if (markColumn.includes(column.property)) {
    if (column.property === 'packageNumber' && row['printedQuantity']) {
      className = 'printed-mark'
    }
    // className += ' marked'
  }
  return className
}

function handleDataFormat({ artifactList, enclosureList, auxList }) {
  const data = {}
  data.artifactList =
    artifactList &&
    artifactList.map((v) => {
      v.weight = v.netWeight || v.grossWeight
      v.totalWeight = convertUnits(v.weight * v.packageQuantity, 'kg', 't')
      v.productQuantity = v.packageQuantity
      return v
    })
  data.enclosureList =
    enclosureList &&
    enclosureList.map((v) => {
      v.processingPrice = v.processingPrice || v.processingPrice === 0 ? v.processingPrice : undefined
      v.totalLength = convertUnits(v.length * v.packageQuantity, 'mm', 'm')
      v.productQuantity = v.packageQuantity
      return v
    })
  data.auxList =
    auxList &&
    auxList.map((v) => {
      v.fullClassName = `${v.firstName}/${v.secondName}/${v.thirdName}`
      v.productQuantity = v.packageQuantity
      return v
    })
  return JSON.stringify(data)
}

async function edit(id, projectId) {
  try {
    const data = (await detail(id)) || {}
    router.push({ name: 'MesManualPack', params: { id, projectId, remark: data.remark, data: handleDataFormat(data) }})
  } catch (error) {
    console.log('去编辑包', error)
  }
}

const del = debounce(
  async function (id) {
    try {
      await crudApi.del(id)
      ElNotification({ title: '删除成功', type: 'success', duration: 2500 })
      crud.toQuery()
    } catch (error) {
      console.log('删除打包清单', error)
    }
  },
  200,
  false
)
</script>

<style lang="scss" scoped>
$default-cell-mask-color: #ff000021 !default;
table {
  .common-button + .common-button {
    margin-left: 5px;
  }
}
::v-deep(.printed-mark) {
  overflow: hidden !important;
  .cell {
    &:after {
      content: '已打印';
      background: #e64242;
      transform: rotate(-45deg);
      color: white;
      font-weight: 100;
      position: absolute;
      top: 5px;
      left: -20px;
      right: 0;
      width: 70px;
      height: 20px;
      font-size: 11px;
      display: flex;
      justify-content: center;
      align-items: center;
      pointer-events: none; // 穿透
    }
  }
}
::v-deep(.marked) {
  .cell {
    padding-left: 0px;
  }
}
</style>
