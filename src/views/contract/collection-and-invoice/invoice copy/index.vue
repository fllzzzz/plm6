<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader id="invoiceHead" ref="header" :permission="permission" />
    <!--表格渲染-->
    <el-table
      ref="table"
      v-loading="crud.loading"
      border
      :stripe="$TBS.STRIPE"
      :data="[{id:1}]"
      :empty-text="crud.emptyText"
      :max-height="$_tableMaxHeight({ head:'', extra:175, hasFixedHeader: hasFixedHeader, paginate:false })"
      style="width: 100%;"
      @sort-change="crud.handleSortChange"
      @selection-change="crud.selectionChangeHandler"
    >
      <!-- <el-table-column type="selection" width="55" align="center" /> -->
      <el-table-column v-if="$TBS.INDEX" :label="$TBS.INDEX_LABEL ? '序号': ''" type="index" align="center" width="60" />
      <el-table-column v-if="columns.visible('payer')" key="payer" prop="payer" sortable="custom" :show-overflow-tooltip="true" label="项目" min-width="120" />
      <el-table-column v-if="columns.visible('invoiceAmount1')" key="invoiceAmount1" prop="invoiceAmount1" sortable="custom" label="合同金额" align="right" min-width="125">
        <template v-slot="scope">
          <!--          <el-tag type="success" effect="plain" style="width:100%;max-width:130px">{{ scope.row.invoiceAmount1 | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>-->
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" sortable="custom" label="开票金额" align="right" min-width="125">
        <template v-slot="scope">
          <el-tag type="success" effect="plain" style="width:100%;max-width:130px">{{ scope.row.invoiceAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('invoiceTypeAndTaxRate')" key="invoiceTypeAndTaxRate" prop="invoiceTypeAndTaxRate" :show-overflow-tooltip="true" label="发票类型" min-width="120" />
      <!--      <el-table-column v-if="columns.visible('applicationDate')" key="applicationDate" prop="applicationDate" sortable="custom" label="申请日期" align="center" width="110">-->
      <!--        <template v-slot="scope">-->
      <!--          <span>{{ scope.row.applicationDate | parseTime('{y}-{m}-{d}') }}</span>-->
      <!--        </template>-->
      <!--      </el-table-column>-->
      <el-table-column v-if="columns.visible('applicationDate')" key="invoiceCollector1" prop="invoiceCollecto1" sortable="custom" :show-overflow-tooltip="true" label="开票单位" min-width="160" />
      <el-table-column v-if="columns.visible('invoiceCollector')" key="invoiceCollector" prop="invoiceCollector" sortable="custom" :show-overflow-tooltip="true" label="收票单位" min-width="160" />
      <!--      <el-table-column v-if="columns.visible('invoiceAmount')" key="invoiceAmount" prop="invoiceAmount" sortable="custom" label="发票面额(元)" align="right" min-width="125">-->
      <!--        <template v-slot="scope">-->
      <!--          <el-tag type="success" effect="plain" style="width:100%;max-width:130px">{{ scope.row.invoiceAmount | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>-->
      <!--        </template>-->
      <!--      </el-table-column>-->
      <!--      <el-table-column v-if="columns.visible('tax')" key="tax" prop="tax" label="税额(元)" align="right" min-width="120">-->
      <!--        <template v-slot="scope">-->
      <!--          <el-tag type="warning" effect="plain" style="width:100%;max-width:120px">{{ scope.row.tax | toFixed($DP.YUAN) | toThousandFilter | emptyTextFormatter }}</el-tag>-->
      <!--        </template>-->
      <!--      </el-table-column>-->
      <el-table-column v-if="columns.visible('applicantName')" key="applicantName" prop="applicantName" :show-overflow-tooltip="true" label="填报人" align="center" min-width="100px" />
      <el-table-column v-if="columns.visible('invoiceDate')" key="invoiceDate" prop="invoiceDate" sortable="custom" label="填报日期" align="center" min-width="140">
        <template v-slot="scope">
          <span>{{ scope.row.invoiceDate | parseTime('{y}-{m}-{d}') }}</span>
        </template>
      </el-table-column>
      <!--      <el-table-column v-if="columns.visible('invoiceUnit')" key="invoiceUnit" prop="invoiceUnit" sortable="custom" :show-overflow-tooltip="true" label="开票单位" align="center" min-width="140" />-->
      <el-table-column v-if="columns.visible('handlerName1')" key="handlerName1" prop="handlerName1" :show-overflow-tooltip="true" label="状态" align="center" width="100px" />
      <el-table-column v-if="columns.visible('dataSourceSys')" key="dataSourceSys" prop="dataSourceSys" :show-overflow-tooltip="true" label="来源" align="center" min-width="100px">
        <template v-slot="scope">
          <span>{{ dataSourceSysEnumV[scope.row.dataSourceSys] && dataSourceSysEnumV[scope.row.dataSourceSys].L | emptyTextFormatter }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('remark')" key="remark" prop="remark" :show-overflow-tooltip="true" label="备注" min-width="150" /> -->
      <!--      <el-table-column v-if="columns.visible('confirmTime')" key="confirmTime" prop="confirmTime" sortable="custom" label="办理时间" align="center" width="160px">-->
      <!--        <template v-slot="scope">-->
      <!--          <span>{{ scope.row.confirmTime | parseTime('{y}-{m}-{d} {h}:{i}') }}</span>-->
      <!--        </template>-->
      <!--      </el-table-column>-->
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" sortable="custom" label="创建时间" align="center" width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.createTime | parseTime }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" sortable="custom" label="更新时间" align="center" width="160px">
        <template v-slot="scope">
          <span>{{ scope.row.updateTime | parseTime }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('lastEditorName')" key="lastEditorName" prop="lastEditorName" label="最后修改人" align="center" width="100px" />
      <!--      <el-table-column-->
      <!--        key="audit"-->
      <!--        label="办理"-->
      <!--        width="90px"-->
      <!--        align="center"-->
      <!--        fixed="right"-->
      <!--      >-->
      <!--        <template v-slot="scope">-->
      <!--          <template v-if="!scope.row.isProcessed">-->
      <!--            <template v-if="checkPermission(permission.audit)">-->
      <!--              <el-button-->
      <!--                v-if="$refs.header && !$refs.header.hasSettlement"-->
      <!--                :disabled="$refs.header.hasSettlement"-->
      <!--                type="warning"-->
      <!--                size="mini"-->
      <!--                icon="el-icon-s-check"-->
      <!--                @click="openCheckDlg(scope.row)"-->
      <!--              />-->
      <!--            </template>-->
      <!--            <el-tag v-else type="primary" size="small">未办理</el-tag>-->
      <!--          </template>-->
      <!--          <span v-else type="success">{{ scope.row.handlerName }}</span>-->
      <!--        </template>-->
      <!--      </el-table-column>-->
      <!--   详情   -->
      <!--      <el-table-column-->
      <!--        key="detail"-->
      <!--        label="详情"-->
      <!--        width="70px"-->
      <!--        align="center"-->
      <!--        fixed="right"-->
      <!--      >-->
      <!--        <template v-slot="scope">-->
      <!--          <el-button type="primary" size="mini" icon="el-icon-tickets" @click="openDetailDlg(scope.row)" />-->
      <!--        </template>-->
      <!--      </el-table-column>-->
      <el-table-column
        v-if="checkPermission(permission.del) && $refs.header && !$refs.header.hasSettlement"
        key="operate"
        label="操作"
        width="120px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <el-button v-permission="permission.detail" size="mini" icon="el-icon-view" type="primary" @click="openDetailDlg(scope.row)" />
          <el-button size="mini" icon="el-icon-s-check" type="primary" @click="openDetailDlg(scope.row,2)" />
        </template>
      </el-table-column>
    </el-table>
    <!--表单渲染-->
    <mForm />
    <mCheck :visible.sync="checkDlgVisible" :record="row" @success="crud.toQuery" />
    <mDetail :visible.sync="detailDlgVisible" :record="detailRow" :type="type" @success="crud.toQuery" />
  </div>
</template>

<script>
import crudMethod, { handleInvoicing } from '@/api/contract/collection/invoice'
import mHeader from './module/header'
import mForm from './module/form'
import mCheck from './module/check'
import mDetail from './module/detail'
import CRUD, { presenter } from '@crud/crud'
// import udOperation from '@crud/UD.operation'
import checkPermission from '@/utils/system/check-permission'
import enumOperate, { dataSourceSysEnum, settlementStatusEnum } from '@/utils/enum/index'
import { validatorInvoiceNo } from '@/utils/validatePattern'

const dataSourceSysEnumV = enumOperate.getVal(dataSourceSysEnum)
// crud交由presenter持有
const permission = {
  get: ['invoice:get'],
  add: ['invoice:add'],
  audit: ['invoice:audit'],
  // edit: ['invoice:edit'],
  del: ['invoice:del'],
  editSettlementStatus: ['collection:editSettlementStatus'],
  download: ['invoice:download'],
  print: ['invoice:print']
}

const optShow = {
  add: true,
  edit: false,
  del: false,
  download: false
}

const crud = CRUD({
  title: '开票信息',
  sort: ['sort.asc', 'id.desc'],
  permission: { ...permission },
  crudMethod: { ...crudMethod },
  optShow: { ...optShow },
  requiredQuery: ['projectId'],
  invisibleColumns: ['invoiceUnit', 'createTime', 'updateTime', 'lastEditorName'],
  hasPagination: false
})

export default {
  components: { mHeader, mForm, mCheck, mDetail },
  mixins: [presenter(crud)],
  inject: ['$_tableMaxHeight'],
  props: {
    projectId: {
      type: [Number, String],
      default: undefined
    },
    hasFixedHeader: {
      type: Boolean,
      default: true
    }
  },
  data() {
    return {
      permission,
      dataSourceSysEnum,
      dataSourceSysEnumV,
      settlementStatusEnum,
      checkDlgVisible: false, // 审核dlg
      detailDlgVisible: false, // 详情dlg
      type: 1,
      row: {}, // 审核行
      detailRow: {} // 详情dlg
    }
  },
  watch: {
    projectId: {
      handler(newVal, oldVal) {
        this.crud.query.projectId = newVal
        this.crud.toQuery()
      }
    }
  },
  mounted() {
    this.crud.query.projectId = this.projectId
    this.crud.toQuery()
  },
  methods: {
    checkPermission,
    // TODO: 无用
    handleInvoicing,
    openCheckDlg(row) {
      this.checkDlgVisible = true
      this.row = row
    },
    openDetailDlg(row, type) {
      this.type = type
      this.detailDlgVisible = true
      this.detailRow = row
    },
    handle(row) {
      let msg
      if (!row.invoiceNo && row.invoiceNo.length > 20) {
        msg = '发票编号长度不可超过20字'
        this.$message.error(msg)
        throw new Error(msg)
      }
      if (!validatorInvoiceNo.test(row.invoiceNo)) {
        msg = '发票编号只能填写数字或字符'
        this.$message.error(msg)
        throw new Error(msg)
      }
      handleInvoicing({ id: row.id, invoiceNo: row.invoiceNo })
    },
    handleSuccess(val) {
      this.crud.toQuery()
    }
  }
}
</script>
